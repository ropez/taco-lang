use std::sync::Arc;

use jiff::{
    RoundMode, Span, Timestamp, Unit, Zoned, ZonedRound,
    civil::{self, Weekday},
    tz::TimeZone,
};

use crate::{
    Builder,
    error::{ScriptResult, TypeResult},
    ext::{ExternalType, ExternalValue, NativeFunction, NativeMethod, NativeMethodRef},
    ident::Ident,
    interpreter::Interpreter,
    script_type::{ScriptType, TupleItemType, TupleType},
    script_value::{ScriptValue, Tuple},
};

pub fn build(builder: &mut Builder) {
    // XXX Need a way to register an ExternalType so that we can refer to it in scripts.
    // e.g. rec Foo(date: DateTime)

    builder.add_function("DateTime::parse", ParseFunc);
    builder.add_function("DateTime::utc_now", UtcNowFunc);
    builder.add_function("DateTime::local", LocalNowFunc);
}

const ZERO_TIME: civil::Time = civil::time(0, 0, 0, 0);

struct DateTimeType;
impl ExternalType for DateTimeType {
    fn name(&self) -> Ident {
        "DateTime".into()
    }

    fn get_method(&self, name: &Ident) -> Option<NativeMethodRef> {
        match name.as_str() {
            "year" => Some(NativeMethodRef::new(Arc::new(GetYearMethod))),
            "month" => Some(NativeMethodRef::new(Arc::new(GetMonthMethod))),
            "day" => Some(NativeMethodRef::new(Arc::new(GetDayMethod))),
            "hour" => Some(NativeMethodRef::new(Arc::new(GetHourMethod))),
            "minute" => Some(NativeMethodRef::new(Arc::new(GetMinuteMethod))),
            "second" => Some(NativeMethodRef::new(Arc::new(GetSecondMethod))),
            "to_iso" => Some(NativeMethodRef::new(Arc::new(ToIsoMethod))),
            "add" => Some(NativeMethodRef::new(Arc::new(AddMethod))),
            "start_of_day" => Some(NativeMethodRef::new(Arc::new(SimpleRoundMethod(
                ZonedRound::from(Unit::Day).mode(RoundMode::Floor),
            )))),
            "end_of_day" => Some(NativeMethodRef::new(Arc::new(SimpleRoundMethod(
                ZonedRound::from(Unit::Day).mode(RoundMode::Ceil),
            )))),
            "start_of_hour" => Some(NativeMethodRef::new(Arc::new(SimpleRoundMethod(
                ZonedRound::from(Unit::Hour).mode(RoundMode::Floor),
            )))),
            "end_of_hour" => Some(NativeMethodRef::new(Arc::new(SimpleRoundMethod(
                ZonedRound::from(Unit::Hour).mode(RoundMode::Ceil),
            )))),
            "start_of_minute" => Some(NativeMethodRef::new(Arc::new(SimpleRoundMethod(
                ZonedRound::from(Unit::Minute).mode(RoundMode::Floor),
            )))),
            "end_of_minute" => Some(NativeMethodRef::new(Arc::new(SimpleRoundMethod(
                ZonedRound::from(Unit::Minute).mode(RoundMode::Ceil),
            )))),
            "start_of_second" => Some(NativeMethodRef::new(Arc::new(SimpleRoundMethod(
                ZonedRound::from(Unit::Second).mode(RoundMode::Floor),
            )))),
            "end_of_second" => Some(NativeMethodRef::new(Arc::new(SimpleRoundMethod(
                ZonedRound::from(Unit::Second).mode(RoundMode::Ceil),
            )))),
            "start_of_week" => Some(NativeMethodRef::new(Arc::new(WeekRoundMethod(-1)))),
            "end_of_week" => Some(NativeMethodRef::new(Arc::new(WeekRoundMethod(1)))),
            _ => None,
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

struct DateTimeValue(Zoned);
impl ExternalValue for DateTimeValue {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

struct LocalNowFunc;

impl NativeFunction for LocalNowFunc {
    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Ext(Arc::new(DateTimeType)))
    }

    fn call(&self, _: &Interpreter, _: &Tuple) -> ScriptResult<ScriptValue> {
        let val = DateTimeValue(Zoned::now());
        Ok(ScriptValue::Ext(Arc::new(DateTimeType), Arc::new(val)))
    }
}

struct ParseFunc;

// For now, parse always return local time.
// (Adjusted by the offset in the input)
impl NativeFunction for ParseFunc {
    fn arguments_type(&self, _: &TupleType) -> TypeResult<TupleType> {
        Ok(TupleType::from_single(ScriptType::Str))
    }

    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        let value_type = ScriptType::Ext(Arc::new(DateTimeType));
        let error_type = ScriptType::Str; // TODO Error type
        Ok(ScriptType::fallible_of(value_type, error_type))
    }

    fn call(&self, _: &Interpreter, arguments: &Tuple) -> ScriptResult<ScriptValue> {
        let arg = arguments.single()?.as_string()?;

        // HACK Get local timezone
        let tz = Zoned::now().time_zone().clone();

        match arg.parse::<Timestamp>() {
            Ok(val) => Ok(ScriptValue::ok(ScriptValue::Ext(
                Arc::new(DateTimeType),
                Arc::new(DateTimeValue(val.to_zoned(tz))),
            ))),
            Err(err) => Ok(ScriptValue::err(ScriptValue::string(format!(
                "Parse error: {err}"
            )))),
        }
    }
}

struct UtcNowFunc;

impl NativeFunction for UtcNowFunc {
    fn return_type(&self, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Ext(Arc::new(DateTimeType)))
    }

    fn call(&self, _: &Interpreter, _: &Tuple) -> ScriptResult<ScriptValue> {
        let val = DateTimeValue(Timestamp::now().to_zoned(TimeZone::UTC));
        Ok(ScriptValue::Ext(Arc::new(DateTimeType), Arc::new(val)))
    }
}

struct GetYearMethod;
impl NativeMethod for GetYearMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn call(&self, _: &Interpreter, s: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let DateTimeValue(val) = s.downcast_ext()?;
        Ok(ScriptValue::Int(val.year() as i64))
    }
}

struct GetMonthMethod;
impl NativeMethod for GetMonthMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn call(&self, _: &Interpreter, s: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let DateTimeValue(val) = s.downcast_ext()?;
        Ok(ScriptValue::Int(val.month() as i64))
    }
}

struct GetDayMethod;
impl NativeMethod for GetDayMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn call(&self, _: &Interpreter, s: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let DateTimeValue(val) = s.downcast_ext()?;
        Ok(ScriptValue::Int(val.day() as i64))
    }
}

struct GetHourMethod;
impl NativeMethod for GetHourMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn call(&self, _: &Interpreter, s: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let DateTimeValue(val) = s.downcast_ext()?;
        Ok(ScriptValue::Int(val.hour() as i64))
    }
}

struct GetMinuteMethod;
impl NativeMethod for GetMinuteMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn call(&self, _: &Interpreter, s: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let DateTimeValue(val) = s.downcast_ext()?;
        Ok(ScriptValue::Int(val.minute() as i64))
    }
}

struct GetSecondMethod;
impl NativeMethod for GetSecondMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Int)
    }

    fn call(&self, _: &Interpreter, s: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let DateTimeValue(val) = s.downcast_ext()?;
        Ok(ScriptValue::Int(val.second() as i64))
    }
}

struct ToIsoMethod;
impl NativeMethod for ToIsoMethod {
    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(ScriptType::Str)
    }

    fn call(&self, _: &Interpreter, s: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let DateTimeValue(val) = s.downcast_ext()?;
        let s = val
            .timestamp()
            .display_with_offset(val.offset())
            .to_string();
        Ok(ScriptValue::string(s))
    }
}

struct SimpleRoundMethod(ZonedRound);
impl NativeMethod for SimpleRoundMethod {
    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(subject.clone())
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let SimpleRoundMethod(round) = self;
        let DateTimeValue(val) = subject.downcast_ext()?;
        let new_val = DateTimeValue(val.round(*round)?);
        Ok(ScriptValue::Ext(Arc::new(DateTimeType), Arc::new(new_val)))
    }
}

struct WeekRoundMethod(i32);
impl NativeMethod for WeekRoundMethod {
    fn return_type(&self, subject: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        Ok(subject.clone())
    }

    fn call(&self, _: &Interpreter, subject: ScriptValue, _: &Tuple) -> ScriptResult<ScriptValue> {
        let WeekRoundMethod(nth) = self;
        let DateTimeValue(val) = subject.downcast_ext()?;
        let date = val.with().time(ZERO_TIME).build()?;
        let date = if *nth < 0 { date.tomorrow()? } else { date };
        let new_val = date.nth_weekday(*nth, Weekday::Monday)?;
        let new_val = DateTimeValue(new_val);
        Ok(ScriptValue::Ext(Arc::new(DateTimeType), Arc::new(new_val)))
    }
}

struct AddMethod;
impl NativeMethod for AddMethod {
    fn arguments_type(&self, _: &ScriptType) -> TypeResult<TupleType> {
        let items = vec![
            TupleItemType::named("nanoseconds", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("milliseconds", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("milliseconds", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("seconds", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("minutes", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("hours", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("days", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("weeks", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("months", ScriptType::opt_of(ScriptType::Int)),
            TupleItemType::named("years", ScriptType::opt_of(ScriptType::Int)),
        ];

        Ok(TupleType::new(items))
    }

    fn return_type(&self, _: &ScriptType, _: &TupleType) -> TypeResult<ScriptType> {
        // Fixme, reuse type
        Ok(ScriptType::Ext(Arc::new(DateTimeType)))
    }

    fn call(
        &self,
        _: &Interpreter,
        subject: ScriptValue,
        args: &Tuple,
    ) -> ScriptResult<ScriptValue> {
        let DateTimeValue(val) = subject.downcast_ext()?;

        let mut span = Span::new();
        let mut iter = args.iter_args();

        if let Some(ScriptValue::Int(v)) = iter.get("nanoseconds") {
            span = span.nanoseconds(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("microseconds") {
            span = span.microseconds(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("milliseconds") {
            span = span.milliseconds(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("seconds") {
            span = span.seconds(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("minutes") {
            span = span.minutes(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("hours") {
            span = span.hours(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("days") {
            span = span.days(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("weeks") {
            span = span.weeks(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("months") {
            span = span.months(v);
        }
        if let Some(ScriptValue::Int(v)) = iter.get("years") {
            span = span.years(v);
        }

        let new_val = DateTimeValue(val.checked_add(span)?);

        Ok(ScriptValue::Ext(Arc::new(DateTimeType), Arc::new(new_val)))
    }
}
