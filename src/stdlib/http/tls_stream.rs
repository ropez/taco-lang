use std::{
    io::{self, Read, Write},
    pin::Pin,
    task::{Context, Poll},
};

use rustls::{ClientConnection, Stream};
use smol::{io::{AsyncRead, AsyncWrite}, net::TcpStream};

/// A TLS stream that implements [AsyncRead] and [AsyncWrite].
pub struct TlsStream {
    connection: ClientConnection,
    stream: TcpStream
}

impl TlsStream {
    pub fn new(connection: ClientConnection, stream: TcpStream) -> TlsStream {
        TlsStream { connection, stream }
    }

    fn get_mut(&mut self) -> (&mut ClientConnection, &mut TcpStream) {
        (&mut self.connection, &mut self.stream)
    }
}

impl AsyncRead for TlsStream
{
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let (connection, stream) = (*self).get_mut();
        let mut stream = Stream {
            conn: connection,
            sock: &mut InnerStream { cx, stream },
        };
        match stream.read(buf) {
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => Poll::Pending,
            res => Poll::Ready(res),
        }
    }

    fn poll_read_vectored(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &mut [std::io::IoSliceMut<'_>],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let (connection, stream) = (*self).get_mut();
        let mut stream = Stream {
            conn: connection,
            sock: &mut InnerStream { cx, stream },
        };
        match stream.read_vectored(bufs) {
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => Poll::Pending,
            res => Poll::Ready(res),
        }
    }
}

impl AsyncWrite for TlsStream
{
    fn poll_write(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let (connection, stream) = (*self).get_mut();
        let mut stream = Stream {
            conn: connection,
            sock: &mut InnerStream { cx, stream },
        };
        match stream.write(buf) {
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => Poll::Pending,
            res => Poll::Ready(res),
        }
    }

    fn poll_write_vectored(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let (connection, stream) = (*self).get_mut();
        let mut stream = Stream {
            conn: connection,
            sock: &mut InnerStream { cx, stream },
        };
        match stream.write_vectored(bufs) {
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => Poll::Pending,
            res => Poll::Ready(res),
        }
    }

    fn poll_flush(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let (connection, stream) = (*self).get_mut();
        let mut stream = Stream {
            conn: connection,
            sock: &mut InnerStream { cx, stream },
        };
        match stream.flush() {
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => Poll::Pending,
            res => Poll::Ready(res),
        }
    }

    fn poll_close(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        self.poll_flush(cx)
    }
}

/// A wrapper that implements synchronous `Read` and `Write` for an asynchronous stream `T`.
/// This is a bridge between the synchronous I/O required by `rustls` and the asynchronous I/O of the underlying stream.
struct InnerStream<'a, 'b, T> {
    cx: &'a mut Context<'b>,
    stream: &'a mut T,
}

impl<'a, 'b, T: AsyncRead + Unpin> Read for InnerStream<'a, 'b, T> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match Pin::new(&mut self.stream).poll_read(self.cx, buf) {
            Poll::Ready(res) => res,
            Poll::Pending => Err(io::ErrorKind::WouldBlock.into()),
        }
    }

    fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
        match Pin::new(&mut self.stream).poll_read_vectored(self.cx, bufs) {
            Poll::Ready(res) => res,
            Poll::Pending => Err(io::ErrorKind::WouldBlock.into()),
        }
    }
}

impl<'a, 'b, T: AsyncWrite + Unpin> Write for InnerStream<'a, 'b, T> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match Pin::new(&mut self.stream).poll_write(self.cx, buf) {
            Poll::Ready(res) => res,
            Poll::Pending => Err(io::ErrorKind::WouldBlock.into()),
        }
    }

    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        match Pin::new(&mut self.stream).poll_write_vectored(self.cx, bufs) {
            Poll::Ready(res) => res,
            Poll::Pending => Err(io::ErrorKind::WouldBlock.into()),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match Pin::new(&mut self.stream).poll_flush(self.cx) {
            Poll::Ready(res) => res,
            Poll::Pending => Err(io::ErrorKind::WouldBlock.into()),
        }
    }
}

