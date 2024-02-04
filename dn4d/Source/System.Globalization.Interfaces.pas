unit System.Globalization.Interfaces;

interface

uses System_;

type
  DateTimeFormatInfo = interface;
  NumberFormatInfo = interface;

  CalendarWeekRule = record
  const
    FirstDay=0;
    FirstFourDayWeek=2;
    FirstFullWeek=1;

  private
    Value: Byte;

  public
    class operator implicit(const AValue: CalendarWeekRule) : Integer;
    class operator implicit(AValue: Integer) : CalendarWeekRule;
    class operator Equal(const L, R: CalendarWeekRule) : Boolean;
    class operator NotEqual(const L, R: CalendarWeekRule) : Boolean;
  end;

  CompareOptions = record
  const
    IgnoreCase=1;
    IgnoreKanaType=8;
    IgnoreNonSpace=2;
    IgnoreSymbols=4;
    IgnoreWidth=$10;
    None=0;
    Ordinal=$40000000;
    OrdinalIgnoreCase=$10000000;
    StringSort=$20000000;

  private
    Value: Integer;

  public
    class operator implicit(const AValue: CompareOptions) : Integer;
    class operator implicit(AValue: Integer) : CompareOptions;
    class operator Equal(const L, R: CompareOptions) : Boolean;
    class operator NotEqual(const L, R: CompareOptions) : Boolean;
    class operator LogicalAnd(const L, R: CompareOptions) : CompareOptions;
    class operator LogicalOr(const L, R: CompareOptions) : CompareOptions;
  end;

  DateTimeFormatFlags = record
  const
    None=0;
    NotInitialized=-1;
    UseDigitPrefixInTokens=$20;
    UseGenitiveMonth=1;
    UseHebrewRule=8;
    UseLeapYearMonth=2;
    UseSpacesInDayNames=$10;
    UseSpacesInMonthNames=4;
  private
    Value: Integer;

  public
    class operator implicit(const AValue: DateTimeFormatFlags) : Integer;
    class operator implicit(AValue: Integer) : DateTimeFormatFlags;
    class operator Equal(const L, R: DateTimeFormatFlags) : Boolean;
    class operator NotEqual(const L, R: DateTimeFormatFlags) : Boolean;
    class operator LogicalAnd(const L, R: DateTimeFormatFlags) : DateTimeFormatFlags;
    class operator LogicalOr(const L, R: DateTimeFormatFlags) : DateTimeFormatFlags;
  end;

  MonthNameStyles = record
  const
    Genitive=1;
    LeapYear=2;
    Regular=0;

  private
    Value: Integer;

  public
    class operator implicit(const AValue: MonthNameStyles) : Integer;
    class operator implicit(AValue: Integer) : MonthNameStyles;
    class operator Equal(const L, R: MonthNameStyles) : Boolean;
    class operator NotEqual(const L, R: MonthNameStyles) : Boolean;
  end;

  Calendar = interface(IBaseInterface)
    ['{06C5FFAC-AD59-46D8-BD11-86FEA544B039}']
    function  get_ID: Integer;
    function  get_BaseCalendarID: Integer;
    function  get_CurrentEraValue: Integer;
    function  get_MaxSupportedDateTime: CDateTime;
    function  get_MinSupportedDateTime: CDateTime;
    function  get_TwoDigitYearMax: Integer;
    procedure set_TwoDigitYearMax(Value: Integer);

    function  GetDayOfWeek(const time: CDateTime): DayOfWeek;
    function  GetYear(const time: CDateTime): Integer;
    function  GetMonth(const time: CDateTime): Integer;
    function  GetDayOfMonth(const time: CDateTime): Integer;
    function  GetEra(const time: CDateTime): Integer;
    function  IsLeapYear(year: Integer): boolean; overload;
    function  IsLeapYear(year: Integer; era: Integer): boolean; overload;
    procedure SetReadOnlyState(readOnly: boolean);
    function  ToFourDigitYear(year: Integer): Integer;
    function  TryToDateTime(  year: Integer;
                              month: Integer;
                              day: Integer;
                              hour: Integer;
                              minute: Integer;
                              second: Integer;
                              millisecond: Integer;
                              era: Integer;
                              var _result: CDateTime): boolean;

    property ID: Integer read get_ID;
    property BaseCalendarID: Integer read get_BaseCalendarID;
    property CurrentEraValue: Integer read get_CurrentEraValue;
    property MaxSupportedDateTime: CDateTime read get_MaxSupportedDateTime;
    property MinSupportedDateTime: CDateTime read get_MinSupportedDateTime;
    property TwoDigitYearMax: Integer read get_TwoDigitYearMax write set_TwoDigitYearMax;
  end;


  CompareInfo = interface(IBaseInterface)
    ['{9CB89F43-AB5D-4352-AAC8-C31E1CBC4123}']
    function Compare( const Str1, Str2: CString): Integer; overload;
    function Compare( const string1: CString;
                      offset1: Integer;
                      length1: Integer;
                      const string2: CString;
                      offset2: Integer;
                      length2: Integer;
                      options: CompareOptions): Integer; overload;
  end;

  // Should have been defined in System.Globalization
  CultureInfo = interface(IFormatProvider)
    ['{EA451234-3229-437E-80D2-111FC0759785}']
    function  get_Calendar: Calendar;
    function  get_IsNeutralCulture: Boolean;
    function  get_DateTimeFormat: DateTimeFormatInfo;
    procedure set_DateTimeFormat(const value: DateTimeFormatInfo);
    function  get_LCID: Integer;
    function  get_Name: CString;
    function  get_NumberFormat: NumberFormatInfo;
    procedure set_NumberFormat(const value: NumberFormatInfo);
    function  get_Parent: CultureInfo;

    property Calendar: Calendar read get_Calendar;
    property IsNeutralCulture: boolean read get_IsNeutralCulture;
    property DateTimeFormat: DateTimeFormatInfo read get_DateTimeFormat write set_DateTimeFormat;
    property LCID: Integer read get_LCID;
    property Name: CString read get_Name;
    property NumberFormat: NumberFormatInfo read get_NumberFormat write set_NumberFormat;
    property Parent: CultureInfo read get_Parent;
  end;

  DateTimeFormatInfo = interface(IBaseInterface)
    ['{ADF5E4FD-EB5B-4C8A-AEFD-533255EC4C91}']
    function  get_AMDesignator: CString;
    procedure set_AMDesignator(const Value: CString);
    function  get_Calendar: Calendar;
    function  get_CompareInfo: CompareInfo;
    procedure set_Calendar(const Value: Calendar);
    function  get_CalendarWeekRule: CalendarWeekRule;
    procedure set_CalendarWeekRule(const Value: CalendarWeekRule);
    function  get_CultureId: Integer;
    function  get_DateSeparator: CString;
    procedure set_DateSeparator(const Value: CString);
    function  get_DateTimeOffsetPattern: CString;
    function  get_EraNames: StringArray;
    function  get_FormatFlags: DateTimeFormatFlags;
    function  get_FullDateTimePattern: CString;
    procedure set_FullDateTimePattern(const Value: CString);
    function  get_GeneralLongTimePattern: CString;
    function  get_GeneralShortTimePattern: CString;
    function  get_HasForceTwoDigitYears: Boolean;
    function  get_HasSpacesInDayNames: Boolean;
    function  get_HasYearMonthAdjustment: Boolean;
    function  get_DayNames : StringArray;
    procedure set_DayNames(const Value: StringArray);
    function  get_LongDatePattern: CString;
    procedure set_LongDatePattern(const Value: CString);
    function  get_LongTimePattern: CString;
    procedure set_LongTimePattern(const Value: CString);
    function  get_MonthDayPattern: CString;
    procedure set_MonthDayPattern(const Value: CString);
    function  get_MonthNames : StringArray;
    procedure set_MonthNames(const Value: StringArray);
    function  get_PMDesignator: CString;
    procedure set_PMDesignator(const Value: CString);
    function  get_RFC1123Pattern: CString;
    function  get_ShortDatePattern: CString;
    procedure set_ShortDatePattern(const Value: CString);
    function  get_ShortTimePattern: CString;
    procedure set_ShortTimePattern(const Value: CString);
    function  get_SortableDateTimePattern: CString;
    function  get_TimeSeparator: CString;
    procedure set_TimeSeparator(const Value: CString);
    function  get_UniversalSortableDateTimePattern: CString;
    function  get_YearMonthPattern: CString;
    procedure set_YearMonthPattern(const Value: CString);

    function Clone: CObject;
    function GetAbbreviatedDayName(dayofweek: DayOfWeek): CString;
    function GetAbbreviatedMonthName(month: Integer): CString;
    function GetDayName(dayofweek: DayOfWeek): CString;
    function GetEraName(era: Integer): CString;
    function GetMonthName(month: Integer): CString;
    function internalGetMonthName(month: Integer; style: MonthNameStyles; abbreviated: boolean): CString;
    function YearMonthAdjustment(var year: Integer; var month: Integer; parsedMonthName: boolean): boolean;

    property AMDesignator: CString read get_AMDesignator write set_AMDesignator;
    property Calendar: Calendar read get_Calendar write set_Calendar;
    property CalendarWeekRule: CalendarWeekRule read get_CalendarWeekRule write set_CalendarWeekRule;
    property CompareInfo: CompareInfo read get_CompareInfo;
    property CultureId: Integer read get_CultureId;
    property DateSeparator: CString read get_DateSeparator write set_DateSeparator;
    property DateTimeOffsetPattern: CString read get_DateTimeOffsetPattern;
    property DayNames: StringArray read get_DayNames write set_DayNames;
    property EraNames: StringArray read get_EraNames;
    property FormatFlags: DateTimeFormatFlags read get_FormatFlags;
    property FullDateTimePattern: CString read get_FullDateTimePattern write set_FullDateTimePattern;
    property GeneralLongTimePattern: CString read get_GeneralLongTimePattern;
    property GeneralShortTimePattern: CString read get_GeneralShortTimePattern;
    property HasForceTwoDigitYears: boolean read get_HasForceTwoDigitYears;
    property HasSpacesInDayNames: boolean read get_HasSpacesInDayNames;
    property HasYearMonthAdjustment: boolean read get_HasYearMonthAdjustment;
    property LongDatePattern: CString read get_LongDatePattern write set_LongDatePattern;
    property LongTimePattern: CString read get_LongTimePattern write set_LongTimePattern;
    property MonthDayPattern: CString read get_MonthDayPattern write set_MonthDayPattern;
    property MonthNames: StringArray read get_MonthNames write set_MonthNames;
    property PMDesignator: CString read get_PMDesignator write set_PMDesignator;
    property RFC1123Pattern: CString read get_RFC1123Pattern;
    property ShortDatePattern: CString read get_ShortDatePattern write set_ShortDatePattern;
    property ShortTimePattern: CString read get_ShortTimePattern write set_ShortTimePattern;
    property SortableDateTimePattern: CString read get_SortableDateTimePattern;
    property TimeSeparator: CString read get_TimeSeparator write set_TimeSeparator;
    property UniversalSortableDateTimePattern: CString read get_UniversalSortableDateTimePattern;
    property YearMonthPattern: CString read get_YearMonthPattern write set_YearMonthPattern;
  end;

  NumberFormatInfo = interface(IBaseInterface)
    ['{5502FB20-4475-4C55-B666-DB1586EB4169}']
    function  get_CurrencySymbol: CString;
    procedure set_CurrencySymbol(const Value: CString);
    function  get_NaNSymbol: CString;
    procedure set_NaNSymbol(const Value: CString);
    function  get_NegativeInfinitySymbol: CString;
    procedure set_NegativeInfinitySymbol(const Value: CString);
    function  get_NumberDecimalSeparator: CString;
    procedure set_NumberDecimalSeparator(const Value: CString);
    function  get_PositiveInfinitySymbol: CString;
    procedure set_PositiveInfinitySymbol(const Value: CString);

    property CurrencySymbol: CString read get_CurrencySymbol write set_CurrencySymbol;
    property NaNSymbol: CString read get_NaNSymbol write set_NaNSymbol;
    property NegativeInfinitySymbol: CString read get_NegativeInfinitySymbol write set_NegativeInfinitySymbol;
    property NumberDecimalSeparator: CString read get_NumberDecimalSeparator write set_NumberDecimalSeparator;
    property PositiveInfinitySymbol: CString read get_PositiveInfinitySymbol write set_PositiveInfinitySymbol;
  end;

implementation

{ CalendarWeekRule }
class operator CalendarWeekRule.implicit(const AValue: CalendarWeekRule) : Integer;
begin
  Result := AValue.Value;
end;

class operator CalendarWeekRule.implicit(AValue: Integer) : CalendarWeekRule;
begin
  Result.Value := Byte(AValue);
end;

class operator CalendarWeekRule.Equal(const L, R: CalendarWeekRule) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator CalendarWeekRule.NotEqual(const L, R: CalendarWeekRule) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

{ CompareOptions }
class operator CompareOptions.implicit(const AValue: CompareOptions) : Integer;
begin
  Result := AValue.Value;
end;

class operator CompareOptions.implicit(AValue: Integer) : CompareOptions;
begin
  Result.Value := AValue;
end;

class operator CompareOptions.Equal(const L, R: CompareOptions) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator CompareOptions.NotEqual(const L, R: CompareOptions) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator CompareOptions.LogicalAnd(const L, R: CompareOptions) : CompareOptions;
begin
  Result := L.Value and R.Value;
end;

class operator CompareOptions.LogicalOr(const L, R: CompareOptions) : CompareOptions;
begin
  Result := L.Value or R.Value;
end;

{ DateTimeFormatFlags }
class operator DateTimeFormatFlags.implicit(const AValue: DateTimeFormatFlags) : Integer;
begin
  Result := AValue.Value;
end;

class operator DateTimeFormatFlags.implicit(AValue: Integer) : DateTimeFormatFlags;
begin
  Result.Value := AValue;
end;

class operator DateTimeFormatFlags.Equal(const L, R: DateTimeFormatFlags) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator DateTimeFormatFlags.NotEqual(const L, R: DateTimeFormatFlags) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

class operator DateTimeFormatFlags.LogicalAnd(const L, R: DateTimeFormatFlags) : DateTimeFormatFlags;
begin
  Result := L.Value and R.Value;
end;

class operator DateTimeFormatFlags.LogicalOr(const L, R: DateTimeFormatFlags) : DateTimeFormatFlags;
begin
  Result := L.Value or R.Value;
end;

{ MonthNameStyles }
class operator MonthNameStyles.implicit(const AValue: MonthNameStyles) : Integer;
begin
  Result := AValue.Value;
end;

class operator MonthNameStyles.implicit(AValue: Integer) : MonthNameStyles;
begin
  Result.Value := AValue;
end;

class operator MonthNameStyles.Equal(const L, R: MonthNameStyles) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator MonthNameStyles.NotEqual(const L, R: MonthNameStyles) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

end.
