{$I Adato.inc}

{$IFDEF CROSSVCL}
{$DEFINE MSWINDOWS}
{$ENDIF}

{$W-}
unit System.Globalization;

interface

uses
  SysUtils,
  System_,
  System.Collections,
  System.Collections.Generic,
  System.Globalization.Interfaces;

const
  LOCALE_RETURN_NUMBER = $20000000;

type
  CultureTableItem = interface(IBaseInterface)
    ['{1832240F-8BF9-4060-BDC9-D23B20C006E0}']
    function  get_Culture: Integer;
    function  get_DataItem: Integer;
    function  get_Name: CString;
    procedure set_Culture(Value: Integer);
    procedure set_DataItem(Value: Integer);
    procedure set_Name(const Value: CString);

    property culture: Integer read get_Culture write set_Culture;
    property dataItem: Integer read get_DataItem write set_DataItem;
    property name: CString read get_Name write set_Name;
  end;

  CCultureTableItem = class(TBaseInterfacedObject)
    _Culture: Integer;
    _DataItem: Integer;
    _Name: CString;

    function  get_Culture: Integer;
    function  get_DataItem: Integer;
    function  get_Name: CString;
    procedure set_Culture(Value: Integer);
    procedure set_DataItem(Value: Integer);
    procedure set_Name(const Value: CString);
  end;

  CultureData = interface(IBaseInterface)
    ['{1D22CD9B-6FA0-4CAE-B3C7-5123430452B5}']

  end;

  CCultureData = class(TBaseInterfacedObject, CultureData)

  end;

  CultureTable = interface(IBaseInterface)
    ['{AB092EBF-073E-47E3-8775-90B6B7E9C84D}']

    function GetDataItemFromCultureID(cultureID: Integer; var actualName: CString): Integer;
    function GetDataItemFromCultureName(const name: CString; var culture: Integer; var actualName: CString): Integer;
  end;

  CCultureTable = class(TBaseInterfacedObject, CultureTable)
  private
    class var HashByName: HashTable;
    class var HashByLcid: HashTable;

    class var m_defaultInstance: CultureTable;
    class function get_Default: CultureTable; static;
    function GetDataItemFromCultureID(cultureID: Integer; var actualName: CString): Integer;
    function GetDataItemFromCultureName(const name: CString; var culture: Integer; var actualName: CString): Integer;
    class property Default: CultureTable read get_Default;

    class procedure _Create; static;
    constructor Create(const fileName: CString; fromAssembly: boolean);
  end;

  CalendarTable = class
  private
    class var _lock: IAutoObject;
    class var m_defaultInstance: CalendarTable;

    class function get_Default: CalendarTable; static;

    class procedure _Create; static;
    constructor Create(const fileName: CString; fromAssembly: boolean);

    class function nativeGetEraName(culture: Integer; calID: Integer): CString; static;

    function SABBREVDAYNAMES(id: Integer): StringArray;
    function SABBREVMONTHNAMES(id: Integer): StringArray;
    function ICURRENTERA(id: Integer): Integer;
    function SDAYNAMES(id: Integer): StringArray;
    function SERANAMES(id: Integer): StringArray;
    function SLEAPYEARMONTHNAMES(id: Integer): StringArray;
    function SLONGDATE(id: Integer): StringArray;
    function SMONTHDAY(id: Integer): CString;
    function SMONTHNAMES(id: Integer): StringArray;
    function SSHORTDATE(id: Integer): StringArray;
    function SYEARMONTH(id: Integer): StringArray;

    class property Default: CalendarTable read get_Default;
  end;

  {$M+}
  CCalendar = class(TBaseInterfacedObject, Calendar)
  private
    _twoDigitYearMax: Integer;
    m_currentEraValue: Integer;
    m_isReadOnly: Boolean;

    function  get_ID: Integer; virtual;
    function  get_BaseCalendarID: Integer;
    function  get_CurrentEraValue: Integer;
    function  get_MaxSupportedDateTime: CDateTime;
    function  get_MinSupportedDateTime: CDateTime;
    function  get_TwoDigitYearMax: Integer;
    procedure set_TwoDigitYearMax(Value: Integer);

    function  GetDayOfWeek(const time: CDateTime): DayOfWeek; virtual; abstract;
    function  GetYear(const time: CDateTime): Integer; virtual; abstract;
    function  GetMonth(const time: CDateTime): Integer; virtual; abstract;
    function  GetDayOfMonth(const time: CDateTime): Integer; virtual; abstract;
    function  GetEra(const time: CDateTime): Integer; virtual; abstract;
    function  IsLeapYear(year: Integer): boolean; overload;
    function  IsLeapYear(year: Integer; era: Integer): boolean; overload; virtual; abstract;
    procedure SetReadOnlyState(readOnly: boolean);

    function ToDateTime(  year: Integer;
                          month: Integer;
                          day: Integer;
                          hour: Integer;
                          minute: Integer;
                          second: Integer;
                          millisecond: Integer;
                          era: Integer): CDateTime; virtual; abstract;

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
  public
    constructor Create;

    property ID: Integer read get_ID;
    property BaseCalendarID: Integer read get_BaseCalendarID;
    property CurrentEraValue: Integer read get_CurrentEraValue;
    property TwoDigitYearMax: Integer read get_TwoDigitYearMax write set_TwoDigitYearMax;
  end;
  {$M-}

  GregorianCalendarTypes = record
  const
    Arabic=10;
    Localized=1;
    MiddleEastFrench=9;
    TransliteratedEnglish=11;
    TransliteratedFrench=12;
    USEnglish=2;

  private
    Value: Integer;

  public
    class operator implicit(const AValue: GregorianCalendarTypes) : Integer;
    class operator implicit(AValue: Integer) : GregorianCalendarTypes;
    class operator Equal(const L, R: GregorianCalendarTypes) : Boolean;
    class operator NotEqual(const L, R: GregorianCalendarTypes) : Boolean;
  end;

  GregorianCalendar = interface(Calendar)
    ['{7E8C6798-3610-4BF4-89C6-B29FFCDCFD55}']
  end;

  CGregorianCalendar = class(CCalendar, Calendar, GregorianCalendar)
  private
    m_type: GregorianCalendarTypes;

    class var m_defaultInstance: GregorianCalendar;

    function GetDatePart(ticks: Int64; part: Integer): Integer;

  protected
    function  get_ID: Integer; override;

  public
    constructor Create; overload;
    constructor Create(const &type: GregorianCalendarTypes); overload;

    class function GetDefaultInstance: Calendar; static;
    function  GetDayOfWeek(const time: CDateTime): DayOfWeek; override;
    function  GetYear(const time: CDateTime): Integer; override;
    function  GetMonth(const time: CDateTime): Integer; override;
    function  GetDayOfMonth(const time: CDateTime): Integer; override;
    function  GetEra(const time: CDateTime): Integer; override;
    function  IsLeapYear(year: Integer; era: Integer): boolean; override;

    function ToDateTime(  year: Integer;
                          month: Integer;
                          day: Integer;
                          hour: Integer;
                          minute: Integer;
                          second: Integer;
                          millisecond: Integer;
                          era: Integer): CDateTime; override;
  end;

  CCompareInfo = class(TBaseInterfacedObject, CompareInfo)
  private
    class var _classLock: IBaseInterface;

  public
    function Compare(const Str1, Str2: CString): Integer; overload;
    function Compare( const string1: CString;
                      offset1: Integer;
                      length1: Integer;
                      const string2: CString;
                      offset2: Integer;
                      length2: Integer;
                      options: CompareOptions): Integer; overload;
    class function GetCompareInfo(culture: Integer): CompareInfo;
  end;

  CultureTableRecord = interface(IBaseInterface)
    ['{41F0E8CB-1C8C-4048-B04A-4E8B187B43B3}']
    function  get_ActualName: CString;
    function  get_CultureID: Integer;
    function  get_CultureName: CString;
    procedure set_CultureName(const Value: CString);
    function  get_ICALENDARTYPE: Integer;
    function  get_ICOMPAREINFO: Cardinal;
    function  get_SDAYNAMES: StringArray;
    function  get_IFIRSTDAYOFWEEK: Integer;
    function  get_IFIRSTWEEKOFYEAR: Integer;
    function  get_IFLAGS: Integer;
    function  get_IsNeutralCulture: Boolean;
    function  get_SABBREVDAYNAMES: StringArray;
    function  get_SABBREVMONTHNAMES: StringArray;
    function  get_SABBREVMONTHGENITIVENAMES: StringArray;
    function  get_S1159: CString;
    function  get_S2359: CString;
    function  get_SADERA: CString;
    function  get_SDATE: CString;
    function  get_ILANGUAGE: Word;
    function  get_SLONGDATE: CString;
    function  get_SNAME: CString;
    function  get_SMONTHDAY: CString;
    function  get_SMONTHGENITIVENAMES: StringArray;
    function  get_SMONTHNAMES: StringArray;
    function  get_SSHORTDATE: CString;
    function  get_STIME: CString;
    function  get_STIMEFORMAT: CString;
    function  get_SSHORTTIME: CString;
    function  get_SYEARMONTH: CString;
    function  get_SYEARMONTHS: StringArray;

    function CloneWithUserOverride(userOverride: boolean): CultureTableRecord;

    property ActualName: CString read get_ActualName;
    property CultureID: Integer read get_CultureID;
    property CultureName: CString read get_CultureName write set_CultureName;
    property ICALENDARTYPE: Integer read get_ICALENDARTYPE;
    property ICOMPAREINFO: Cardinal read get_ICOMPAREINFO;
    property SDAYNAMES: StringArray read get_SDAYNAMES;
    property IFIRSTDAYOFWEEK: Integer read get_IFIRSTDAYOFWEEK;
    property IFIRSTWEEKOFYEAR: Integer read get_IFIRSTWEEKOFYEAR;
    property IFLAGS: Integer read get_IFLAGS;
    property IsNeutralCulture: boolean read get_IsNeutralCulture;
    property SABBREVDAYNAMES: StringArray read get_SABBREVDAYNAMES;
    property SABBREVMONTHNAMES: StringArray read get_SABBREVMONTHNAMES;
    property SABBREVMONTHGENITIVENAMES: StringArray read get_SABBREVMONTHGENITIVENAMES;
    property S1159: CString read get_S1159;
    property S2359: CString read get_S2359;
    property SADERA: CString read get_SADERA;
    property SDATE: CString read get_SDATE;
    property ILANGUAGE: Word read get_ILANGUAGE;
    property SLONGDATE: CString read get_SLONGDATE;
    property SNAME: CString read get_SNAME;
    property SMONTHDAY: CString read get_SMONTHDAY;
    property SMONTHNAMES: StringArray read get_SMONTHNAMES;
    property SMONTHGENITIVENAMES: StringArray read get_SMONTHGENITIVENAMES;
    property SSHORTDATE: CString read get_SSHORTDATE;
    property STIME: CString read get_STIME;
    property STIMEFORMAT: CString read get_STIMEFORMAT;
    property SSHORTTIME: CString read get_SSHORTTIME;
    property SYEARMONTH: CString read get_SYEARMONTH;
    property SYEARMONTHS: StringArray read get_SYEARMONTHS;
  end;

  CultureTableRecordArray = array of CultureTableRecord;

  CCultureTableRecord = class(TBaseInterfacedObject, CultureTableRecord)
  private
    class var syncObj: TObject;
    class var CultureTableRecordCache: Dictionary<CString, CultureTableRecordArray>;
    class var SyntheticLcidToNameCache: HashTable;
    class var SyntheticNameToLcidCache: HashTable;

  private
    m_ActualName: CString;
    m_ActualCultureID: Integer;
    m_CultureID: Integer;
    m_bUseUserOverride: Boolean;
    m_CultureName: CString;
    m_CultureTable: CultureTable;
    m_synthetic: Boolean;

    class function get_InternalSyncObject: TObject; static;
    function  get_ActualName: CString;
    function  get_CultureID: Integer;
    function  get_CultureName: CString;
    procedure set_CultureName(const Value: CString);
    function  get_ICALENDARTYPE: Integer;
    function  get_ICOMPAREINFO: Cardinal;
    function  get_SDAYNAMES: StringArray;
    function  get_IFIRSTDAYOFWEEK: Integer;
    function  get_IFIRSTWEEKOFYEAR: Integer;
    function  get_IFLAGS: Integer;
    function  get_IsNeutralCulture: Boolean;
    function  get_SABBREVDAYNAMES: StringArray;
    function  get_SABBREVMONTHNAMES: StringArray;
    function  get_SABBREVMONTHGENITIVENAMES: StringArray;
    function  get_S1159: CString;
    function  get_S2359: CString;
    function  get_SADERA: CString;
    function  get_SDATE: CString;
    function  get_ILANGUAGE: Word;
    function  get_SLONGDATE: CString;
    function  get_SNAME: CString;
    function  get_SMONTHDAY: CString;
    function  get_SMONTHGENITIVENAMES: StringArray;
    function  get_SMONTHNAMES: StringArray;
    function  get_SSHORTDATE: CString;
    function  get_STIME: CString;
    function  get_STIMEFORMAT: CString;
    function  get_SSHORTTIME: CString;
    function  get_SYEARMONTH: CString;
    function  get_SYEARMONTHS: StringArray;

    function CloneWithUserOverride(userOverride: boolean): CultureTableRecord;
    class function GetCultureTableRecord({const} name: CString; useUserOverride: boolean): CultureTableRecord; overload; static;
    class function GetCultureTableRecord(cultureId: Integer; useUserOverride: boolean): CultureTableRecord; overload;static;
//    function GetDataItemFromCultureID(cultureID: Integer; var actualName: CString): Integer;
    function GetString(LCType: Cardinal; maxLength: Integer): CString;
    function GetSyntheticCulture(cultureID: Integer): boolean;
    class procedure InitSyntheticMapping; static;
    class function IsCustomCultureId(cultureId: Integer): boolean; static;
    class function StoreLCIDNamePair(cultureID: Integer) : CString; static;
    class function ValidateCulturePieceToLower(const testString: CString; const paramName: CString; maxLength: Integer): CString; static;

    class property InternalSyncObject: TObject read get_InternalSyncObject;
  public
    constructor Create(cultureId: Integer; useUserOverride: boolean); overload;
    constructor Create(const cultureName: CString; useUserOverride: boolean); overload;

    property ActualName: CString read get_ActualName;
    property CultureID: Integer read get_CultureID;
    property CultureName: CString read get_CultureName write set_CultureName;
    property ICALENDARTYPE: Integer read get_ICALENDARTYPE;
    property ICOMPAREINFO: Cardinal read get_ICOMPAREINFO;
    property SDAYNAMES: StringArray read get_SDAYNAMES;
    property IFIRSTDAYOFWEEK: Integer read get_IFIRSTDAYOFWEEK;
    property IFIRSTWEEKOFYEAR: Integer read get_IFIRSTWEEKOFYEAR;
    property IFLAGS: Integer read get_IFLAGS;
    property IsNeutralCulture: boolean read get_IsNeutralCulture;
    property SABBREVDAYNAMES: StringArray read get_SABBREVDAYNAMES;
    property SABBREVMONTHNAMES: StringArray read get_SABBREVMONTHNAMES;
    property SABBREVMONTHGENITIVENAMES: StringArray read get_SABBREVMONTHGENITIVENAMES;
    property S1159: CString read get_S1159;
    property S2359: CString read get_S2359;
    property SADERA: CString read get_SADERA;
    property SDATE: CString read get_SDATE;
    property ILANGUAGE: Word read get_ILANGUAGE;
    property SLONGDATE: CString read get_SLONGDATE;
    property SNAME: CString read get_SNAME;
    property SMONTHDAY: CString read get_SMONTHDAY;
    property SMONTHNAMES: StringArray read get_SMONTHNAMES;
    property SMONTHGENITIVENAMES: StringArray read get_SMONTHGENITIVENAMES;
    property SSHORTDATE: CString read get_SSHORTDATE;
    property STIME: CString read get_STIME;
    property STIMEFORMAT: CString read get_STIMEFORMAT;
    property SSHORTTIME: CString read get_SSHORTTIME;
    property SYEARMONTH: CString read get_SYEARMONTH;
    property SYEARMONTHS: StringArray read get_SYEARMONTHS;
  end;

  CCultureInfo = class(TBaseInterfacedObject, CultureInfo, IFormatProvider)
  private
    class var m_InvariantCultureInfo: CultureInfo;
    class var m_userDefaultCulture: CultureInfo;
    class var m_userDefaultUICulture: CultureInfo;

  private
    _calendar: Calendar;
    cultureID: Integer;
    m_cultureTableRecord: CultureTableRecord;
    m_isReadOnly: boolean;
    m_name: CString;
    dateTimeInfo: DateTimeFormatInfo;
    numInfo: NumberFormatInfo;
    m_isInherited: boolean;

    strict private m_nonSortName: CString;

  protected
    class procedure CheckNeutral(culture: CultureInfo); static;
    class function  InitUserDefaultCulture: CultureInfo;
    class function  InitUserDefaultUICulture: CultureInfo;
    class function  nativeGetCultureName(lcid: Integer; useSNameLCType: boolean; getMonthName: boolean): CString; static;
    class function  nativeGetSystemDefaultUILanguage(var LCID: Integer): CString; static;
    class function  nativeGetUserDefaultLCID(var LCID: Integer; lcidType: Integer): CString; static;
    class function  nativeGetUserDefaultUILanguage(var LCID: Integer): CString; static;
    class function  GetCalendarInstance(calType: Integer): Calendar; static;
    class function  GetCalendarInstanceRare(calType: Integer): Calendar; static;
    class function  GetCultureByLCIDOrName(preferLCID: Integer; const fallbackToString: CString): CultureInfo; static;

    function  get_Calendar: Calendar;
    function  GetFormat(const formatType: &Type): IBaseInterface; overload;
    function  GetFormat(AClass: TClass): IBaseInterface; overload;
    function  get_DateTimeFormat: DateTimeFormatInfo;
    procedure set_DateTimeFormat(const Value: DateTimeFormatInfo);
    function  get_IsNeutralCulture: boolean;
    function  get_LCID: Integer;
    function  get_Parent: CultureInfo;
    function  get_Name: CString;
    function  get_NumberFormat: NumberFormatInfo;
    procedure set_NumberFormat(const value: NumberFormatInfo);

    procedure VerifyWritable;

  public
    class function CurrentCulture: CultureInfo;
    class function CurrentUICulture: CultureInfo;
    class function InvariantCulture: CultureInfo;
    class function UserDefaultCulture: CultureInfo;
    class function UserDefaultUICulture: CultureInfo;

    class procedure _Create; static;
    constructor Create(culture: Integer; useUserOverride: boolean); overload;
    constructor Create(culture: Integer); overload;

    class function GetLangID(culture: Integer): Integer; static;
    class function GetSortID(lcid: Integer): Integer; static;
    class function IsValidLCID(LCID: Integer; flag: Integer): boolean; static;
    class function VerifyCultureName(culture: CultureInfo; throwException: boolean): boolean;

    public property Calendar: Calendar
      read get_Calendar;
    property LCID: Integer
      read get_LCID;
    public property DateTimeFormat: DateTimeFormatInfo
      read  get_DateTimeFormat
      write set_DateTimeFormat;
    public property IsNeutralCulture: boolean
      read get_IsNeutralCulture;
    public property Name: CString read get_Name;
    property Parent: CultureInfo read get_Parent;
    property NumberFormat: NumberFormatInfo read get_NumberFormat write set_NumberFormat;
  end;

  CDateTimeFormatInfo = class(
    TBaseInterfacedObject,
    DateTimeFormatInfo,
    ICloneable,
    IFormatProvider)
  private
    allLongDatePatterns: StringArray;
    _abbreviatedDayNames: StringArray;
    _abbreviatedMonthNames: StringArray;
    _calendarWeekRule: CalendarWeekRule;
    _amDesignator: CString;
    _calendar: Calendar;
    _dateSeparator: CString;
    _dateTimeOffsetPattern: CString;
    _fullDateTimePattern: CString;
    _genitiveMonthNames: StringArray;
    _leapYearMonthNames: StringArray;
    _longDatePattern: CString;
    _longTimePattern: CString;
    _monthDayPattern: CString;
    _pmDesignator: CString;
    _generalLongTimePattern: CString;
    _generalShortTimePattern: CString;
    _shortDatePattern: CString;
    _shortTimePattern: CString;
    _timeSeparator: CString;
    _firstDayOfWeek: Integer;
    _yearMonthPattern: CString;
    formatFlags: DateTimeFormatFlags;
    _dayNames: StringArray;
    _monthNames: StringArray;
    m_eraNames: StringArray;
    m_isDefaultCalendar: Boolean;
    m_genitiveAbbreviatedMonthNames: StringArray;
    m_compareInfo: CompareInfo;
    m_cultureTableRecord: CultureTableRecord;
    m_isReadOnly: boolean;

//    class var _CurrentInfo: DateTimeFormatInfo;
    class var _InvariantInfo: DateTimeFormatInfo;

  protected
    function  get_AMDesignator: CString;
    procedure set_AMDesignator(const Value: CString);
    function  get_Calendar: Calendar;
    procedure set_Calendar(const Value: Calendar);
    function  get_CalendarWeekRule: CalendarWeekRule;
    procedure set_CalendarWeekRule(const Value: CalendarWeekRule);
    function  get_CompareInfo: CompareInfo;
    function  get_DayNames : StringArray;
    procedure set_DayNames(const Value: StringArray);
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

    procedure ClearTokenHashTable(scanDateWords: boolean);
    function  Clone: CObject;
    function  GetAbbreviatedDayName(dayofweek: DayOfWeek): CString;
    function  GetAbbreviatedDayOfWeekNames: StringArray;
    function  GetAbbreviatedMonthName(month: Integer): CString;
    function  GetAbbreviatedMonthNames: StringArray;
    function  GetDayName(dayofweek: DayOfWeek): CString;
    function  GetDayOfWeekNames: StringArray;
    function  GetEraName(era: Integer): CString;
    function  GetMonthName(month: Integer): CString;
    function  GetMonthNames: StringArray;
    function  GetLongDatePattern(calID: Integer): CString;
    function  GetShortDatePattern(calID: Integer): CString;
    function  GetYearMonthPattern(calID: Integer): CString;
    function  internalGetMonthName(month: Integer; style: MonthNameStyles; abbreviated: boolean): CString;
    function  internalGetGenitiveMonthNames(abbreviated: boolean): StringArray;
    function  internalGetLeapYearMonthNames: StringArray;
    procedure InitializeOverridableProperties;
    procedure SetDefaultPatternAsFirstItem(const patterns: StringArray; const defaultPattern: CString);
    procedure VerifyWritable;
    function  YearMonthAdjustment(var year: Integer; var month: Integer; parsedMonthName: boolean): boolean;

    function GetFormat(AClass: TClass): IBaseInterface; overload;
    function GetFormat(const AType: &Type): IBaseInterface; overload;

    class function get_CurrentInfo: DateTimeFormatInfo; static;
    class function get_InvariantInfo: DateTimeFormatInfo; static;

  public
    constructor Create; overload;
    constructor Create(const cultureTable: CultureTableRecord; cultureID: Integer; const cal: Calendar); overload;

    class function GetInstance(const provider: IFormatProvider): DateTimeFormatInfo; static;

    property Calendar: Calendar read get_Calendar write set_Calendar;
    property CultureId: Integer read get_CultureId;
    class property CurrentInfo: DateTimeFormatInfo read get_CurrentInfo;
    property EraNames: StringArray read get_EraNames;
    class property InvariantInfo: DateTimeFormatInfo read get_InvariantInfo;
    property LongDatePattern: CString read get_LongDatePattern write set_LongDatePattern;
    property LongTimePattern: CString read get_LongTimePattern write set_LongTimePattern;
    property ShortDatePattern: CString read get_ShortDatePattern write set_ShortDatePattern;
    property ShortTimePattern: CString read get_ShortTimePattern write set_ShortTimePattern;
  end;

  DateTimeFormat = class
  private
    class var allStandardFormats: CharArray;
    class var fixedNumberFormats: StringArray;
    class var NullOffset: CTimeSpan;

    class procedure _Create; static;

    class function FormatCustomized(  const dateTime: CDateTime;
                                      const format: CString;
                                      const dtfi: DateTimeFormatInfo;
                                      const offset: CTimeSpan): CString;

    class procedure FormatCustomizedRoundripTimeZone( const dateTime: CDateTime;
                                                      {const} offset: CTimeSpan;
                                                      const _result: StringBuilder);

    class procedure FormatCustomizedTimeZone( {const} dateTime: CDateTime;
                                              {const} offset: CTimeSpan;
                                              const format: CString;
                                              tokenLen: Integer;
                                              timeOnly: boolean;
                                              const _result: StringBuilder);

    class function FormatDayOfWeek(_dayOfWeek: Integer; _repeat: Integer; const dtfi: DateTimeFormatInfo): CString;

    class procedure FormatDigits( const outputBuffer: StringBuilder;
                                  value: Integer;
                                  len: Integer);

    class function  FormatHebrewMonthName(  const time: CDateTime;
                                            month: Integer;
                                            repeatCount: Integer;
                                            const dtfi: DateTimeFormatInfo): CString;

    class function  FormatMonth(  month: Integer;
                                  repeatCount: Integer;
                                  const dtfi: DateTimeFormatInfo): CString;

    class procedure HebrewFormatDigits(const outputBuffer: StringBuilder; digits: Integer);

    class procedure InvalidFormatForUtc(const format: CString; const dateTime: CDateTime);

    class function  IsUseGenitiveForm(  const format: CString;
                                        index: Integer;
                                        tokenLen: Integer;
                                        const patternToMatch: CChar): boolean;

    class function ParseNextChar(const format: CString; pos: Integer): Integer;

    class function ParseQuoteString(  const format: CString;
                                      pos: Integer;
                                      const _result: StringBuilder): Integer;

    class function ParseRepeatPattern(  const format: CString;
                                        pos: Integer;
                                        const patternChar: CChar): Integer;

  public
    class function ExpandPredefinedFormat(  const format: CString;
                                            var dateTime: CDateTime;
                                            var dtfi: DateTimeFormatInfo;
                                            var offset: CTimeSpan): CString;

    class function Format(  const DateTime: CDateTime;
                            const format: CString;
                            dtfi: DateTimeFormatInfo) : CString; overload;

    class function Format(  {const} DateTime: CDateTime;
                            {const} format: CString;
                            {const} dtfi: DateTimeFormatInfo;
                            {const} Offset: CTimeSpan) : CString; overload;

    class function  GetRealFormat(  const format: CString;
                                    const dtfi: DateTimeFormatInfo): CString;

    class procedure InvalidFormatForLocal(  const format: CString;
                                            const dateTime: CDateTime);
  end;

  HebrewNumber = class
  public
    class function ToString(Number: Integer): CString; {$IFDEF DELPHI9_UP}reintroduce;{$ENDIF} static;
  end;

  CNumberFormatInfo = class(TBaseInterfacedObject, NumberFormatInfo)
  private
    _CurrencySymbol: CString;
    _NaNSymbol: CString;
    _NegativeInfinitySymbol: CString;
    _numberDecimalSeparator: CString;
    _PositiveInfinitySymbol: CString;

    class var _CurrentInfo: NumberFormatInfo;

    class function get_CurrentInfo: NumberFormatInfo; static;

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

  public
    constructor Create;
    destructor Destroy; override;

    class function GetInstance(const formatProvider: IFormatProvider): NumberFormatInfo;

    class property CurrentInfo: NumberFormatInfo read get_CurrentInfo;
    property NumberDecimalSeparator: CString read get_NumberDecimalSeparator write set_NumberDecimalSeparator;
  end;

//  function GetUserDefaultLocaleName(lpLocaleName: PWideChar; cchLocaleName: Integer): Integer; external 'kernel32.dll' name 'GetUserDefaultLocaleName';
  {$IFDEF MSWINDOWS}
  {$IFNDEF CROSSVCL}
  function GetSystemDefaultUILanguage: Integer; external 'kernel32.dll' name 'GetSystemDefaultUILanguage';
  function GetUserDefaultUILanguage: Integer; external 'kernel32.dll' name 'GetUserDefaultUILanguage';
  {$ENDIF}
  {$ENDIF}

  procedure GlobalInitialization;
  procedure GlobalFinalization;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ELSE}
  FMX.Platform,
  {$ENDIF}
  System_.Threading;

{ GregorianCalendarTypes }
class operator GregorianCalendarTypes.implicit(const AValue: GregorianCalendarTypes) : Integer;
begin
  Result := AValue.Value;
end;

class operator GregorianCalendarTypes.implicit(AValue: Integer) : GregorianCalendarTypes;
begin
  Result.Value := AValue;
end;

class operator GregorianCalendarTypes.Equal(const L, R: GregorianCalendarTypes) : Boolean;
begin
  Result := L.Value = R.Value;
end;

class operator GregorianCalendarTypes.NotEqual(const L, R: GregorianCalendarTypes) : Boolean;
begin
  Result := L.Value <> R.Value;
end;

{CCultureTable}
class procedure CCultureTable._Create;
begin
  m_defaultInstance := CCultureTable.Create('culture.nlp', true);
end;

constructor CCultureTable.Create(const fileName: CString; fromAssembly: boolean);
begin
//  if (inherited IsValid) then
//  begin
    self.hashByName := CHashtable.Create;
    self.hashByLcid := CHashtable.Create;
//    self.hashByRegionName := Hashtable.Create;
//    self.m_pCultureNameIndex := ((inherited m_pDataFileStart + inherited m_pCultureHeader.cultureNameTableOffset) as CultureNameOffsetItem*);
//    self.m_pRegionNameIndex := ((inherited m_pDataFileStart + inherited m_pCultureHeader.regionNameTableOffset) as RegionNameOffsetItem*);
//    self.m_pCultureIDIndex := ((inherited m_pDataFileStart + inherited m_pCultureHeader.cultureIDTableOffset) as IDOffsetItem*)
//  end
end;

class function CCultureTable.get_Default: CultureTable;
begin
  if (CCultureTable.m_defaultInstance = nil) then
    raise CException.Create('Failure has occurred while loading a type.');
  begin
    Result := CCultureTable.m_defaultInstance;
    exit
  end
end;

function CCultureTable.GetDataItemFromCultureID(cultureID: Integer; var actualName: CString): Integer;
var
  item: CultureTableItem;
//  num: Integer;
//  num2: Integer;
begin
  item := IBaseInterface(self.hashByLcid.Item[cultureID]) as CultureTableItem;
  if ((item <> nil) and (item.culture <> 0)) then
  begin
    actualName := item.name;
    begin
      Result := item.dataItem;
      exit
    end
  end;
//  num := 0;
//  num2 := (inherited m_pCultureHeader.numCultureNames - 1);
//  while ((num <= num2)) do
//  begin
//    index := ((num + num2) div 2);
//    num4 := (cultureID - self.m_pCultureIDIndex[index].actualCultureID);
//    if (num4 = 0) then
//    begin
//      item := CultureTableItem.Create;
//      num5 := item.dataItem := self.m_pCultureIDIndex[index].dataItemIndex;
//      item.culture := cultureID;
//      actualName := item.name := inherited GetStringPoolString(self.m_pCultureIDIndex[index].strOffset);
//      self.hashByLcid.Item[cultureID] := item;
//      begin
//        Result := num5;
//        exit
//      end
//    end;
//    if (num4 < 0) then
//      num2 := (index - 1)
//    else
//      num := (index + 1)
//    end;
//  actualName := '';
  begin
    Result := -1;
    exit
  end
end;

function CCultureTable.GetDataItemFromCultureName(const name: CString; var culture: Integer; var actualName: CString): Integer;
var
  item: CultureTableItem;
//  num: Integer;
//  num2: Integer;
begin
  culture := -1;
  actualName := '';
  item := IBaseInterface(self.hashByName.Item[name]) as CultureTableItem;
  if ((item <> nil) and (item.culture <> 0)) then
  begin
    culture := item.culture;
    actualName := item.name;
    begin
      Result := item.dataItem;
      exit
    end
  end;

  Result := -1;
//  item := CCultureTableItem.Create;
//  num5 := item.dataItem := self.m_pCultureNameIndex[index].dataItemIndex;
//  culture := item.culture := self.m_pCultureNameIndex[index].actualCultureID;
//  actualName := item.name := inherited GetStringPoolString(self.m_pCultureNameIndex[index].strOffset);
//  self.hashByName.Item[name] := item;
//  begin
//    Result := num5;
//    exit
//  end
end;

{ CCultureTableItem }
function CCultureTableItem.get_Culture: Integer;
begin
  Result := _Culture;
end;

function CCultureTableItem.get_DataItem: Integer;
begin
  Result := _DataItem;
end;

function CCultureTableItem.get_Name: CString;
begin
  Result := _Name;
end;

procedure CCultureTableItem.set_Culture(Value: Integer);
begin
  _Culture := Value;
end;

procedure CCultureTableItem.set_DataItem(Value: Integer);
begin
  _DataItem := Value;
end;

procedure CCultureTableItem.set_Name(const Value: CString);
begin
  _Name := Value;
end;

{ CalendarTable }
class procedure CalendarTable._Create;
begin
  CalendarTable.m_defaultInstance := CalendarTable.Create('culture.nlp', true);
  _lock := AutoObject.Guard(CalendarTable.m_defaultInstance);
end;

constructor CalendarTable.Create(const fileName: CString; fromAssembly: boolean);
begin

end;

class function CalendarTable.nativeGetEraName(culture: Integer; calID: Integer): CString;
begin

end;

class function CalendarTable.get_Default: CalendarTable;
begin
  Result := CalendarTable.m_defaultInstance
end;

function CalendarTable.SABBREVDAYNAMES(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
end;

function CalendarTable.SABBREVMONTHNAMES(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
end;

function CalendarTable.ICURRENTERA(id: Integer): Integer;
begin
  raise NotImplementedException.Create;
//  Result := self.m_calendars[id].iCurrentEra
end;

function CalendarTable.SDAYNAMES(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
end;

function CalendarTable.SERANAMES(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
//  Result := inherited GetStringArray(self.m_calendars[id].saEraNames)
end;

function CalendarTable.SLEAPYEARMONTHNAMES(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
end;

function CalendarTable.SLONGDATE(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
//  Result := inherited GetStringArray(self.m_calendars[id].saLongDate)
end;

function CalendarTable.SSHORTDATE(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
//  Result := inherited GetStringArray(self.m_calendars[id].saShortDate)
end;

function CalendarTable.SMONTHDAY(id: Integer): CString;
begin
  raise NotImplementedException.Create;
end;

function CalendarTable.SMONTHNAMES(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
end;

function CalendarTable.SYEARMONTH(id: Integer): StringArray;
begin
  raise NotImplementedException.Create;
// Result := inherited GetStringArray(self.m_calendars[id].saYearMonth)
end;

{ CCalendar }
constructor CCalendar.Create;
begin
  inherited;
  self.m_currentEraValue := -1;
  self.twoDigitYearMax := -1;
end;

function CCalendar.get_ID: Integer;
begin
  Result := -1;
end;

function CCalendar.get_BaseCalendarID: Integer;
begin
  Result := self.ID
end;

function CCalendar.get_CurrentEraValue: Integer;
begin
 if (self.m_currentEraValue = -1) then
    self.m_currentEraValue := CalendarTable.Default.ICURRENTERA(self.BaseCalendarID);
  begin
    Result := self.m_currentEraValue;
    exit
  end
end;

function CCalendar.get_MaxSupportedDateTime: CDateTime;
begin
  Result := CDateTime.MaxValue;
end;

function CCalendar.get_MinSupportedDateTime: CDateTime;
begin
  Result := CDateTime.MinValue;
end;

function  CCalendar.get_TwoDigitYearMax: Integer;
begin
  Result := self._twoDigitYearMax;
end;

procedure CCalendar.set_TwoDigitYearMax(Value: Integer);
begin

end;

function CCalendar.IsLeapYear(year: Integer): boolean;
begin
  Result := self.IsLeapYear(year, 0);
end;

function CCalendar.ToFourDigitYear(year: Integer): Integer;
begin
  if (year < 0) then
    raise ArgumentOutOfRangeException.Create('year', Environment.GetResourceString('ArgumentOutOfRange_NeedNonNegNum'));
  if (year < 100) then
    begin
      if (year > (self.TwoDigitYearMax mod 100)) then
        Result := ((((self.TwoDigitYearMax div 100) -  1) * 100) + year) else
        Result := ((((self.TwoDigitYearMax div 100) -  0) * 100) + year);
      exit
    end;
  begin
    Result := year;
    exit
  end
end;

procedure CCalendar.SetReadOnlyState(readOnly: boolean);
begin
  self.m_isReadOnly := readOnly;
end;

function  CCalendar.TryToDateTime(
  year: Integer;
  month: Integer;
  day: Integer;
  hour: Integer;
  minute: Integer;
  second: Integer;
  millisecond: Integer;
  era: Integer;
  var _result: CDateTime): boolean;
begin
  _result := CDateTime.MinValue;
  try
    _result := self.ToDateTime(year, month, day, hour, minute, second, millisecond, era);
    begin
      Result := true;
      exit
    end
  except
    on exception1: ArgumentException do
      begin
        Result := false;
        exit
      end
  end
end;

{ CGregorianCalendar }
constructor CGregorianCalendar.Create;
begin
  inherited;
  self.m_type := GregorianCalendarTypes.Localized;
end;

constructor CGregorianCalendar.Create(const &type: GregorianCalendarTypes);
begin
//  if ((&type < GregorianCalendarTypes.Localized) or (&type > GregorianCalendarTypes.TransliteratedFrench)) then
//    raise ArgumentOutOfRangeException.Create('type', string.Format(CultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_Range'), New(array[2] of TObject, ( ( GregorianCalendarTypes.Localized, GregorianCalendarTypes.TransliteratedFrench ) ))));
  inherited Create;
  self.m_type := &type
end;

function CGregorianCalendar.get_ID: Integer;
begin
  Result := Integer(self.m_type)
end;

class function CGregorianCalendar.GetDefaultInstance: Calendar;
begin
  if (CGregorianCalendar.m_defaultInstance = nil) then
    CGregorianCalendar.m_defaultInstance := CGregorianCalendar.Create;

  Result := CGregorianCalendar.m_defaultInstance;
end;

function CGregorianCalendar.GetDatePart(ticks: Int64; part: Integer): Integer;
var
  index: Integer;
  num: Integer;
  num2: Integer;
  num3: Integer;
  num4: Integer;
  num5: Integer;
//  num6: Integer;
  numArray: CDateTime.FixedDayArray;

begin
  num := (ticks div $c92a69c000);
  num2 := (num div $23ab1);
  dec(num, (num2 * $23ab1));
  num3 := (num div $8eac);
  if (num3 = 4) then
    num3 := 3;
  dec(num, (num3 * $8eac));
  num4 := (num div $5b5);
  dec(num, (num4 * $5b5));
  num5 := (num div $16d);
  if (num5 = 4) then
    num5 := 3;
  if (part = 0) then
    begin
      Result := (((((num2 * 400) + (num3 * 100)) + (num4 * 4)) + num5) + 1);
      exit
    end;
  dec(num, (num5 * $16d));
  if (part = 1) then
    begin
      Result := (num + 1);
      exit
    end;

  if ((num5 = 3) and ((num4 <> $18) or (num3 = 3))) then
    numArray :=  CDateTime.DaysToMonth366 else
    numArray :=  CDateTime.DaysToMonth365;

  index := (num shr 6);
  while ((num >= numArray[index])) do
  begin
    inc(index)
  end;
  if (part = 2) then
    begin
      Result := index;
      exit
    end;
  begin
    Result := ((num - numArray[(index - 1)]) + 1);
    exit
  end
end;

function CGregorianCalendar.GetDayOfWeek(const time: CDateTime): DayOfWeek;
begin
  Result := DayOfWeek(Integer(((time.Ticks div $c92a69c000) + 1) mod 7));
end;

function CGregorianCalendar.GetYear(const time: CDateTime): Integer;
begin
  Result := self.GetDatePart(time.Ticks, 0);
end;

function CGregorianCalendar.GetMonth(const time: CDateTime): Integer;
begin
  Result := self.GetDatePart(time.Ticks, 2)
end;

function CGregorianCalendar.GetDayOfMonth(const time: CDateTime): Integer;
begin
  Result := self.GetDatePart(time.Ticks, 3)
end;

function CGregorianCalendar.GetEra(const time: CDateTime): Integer;
begin
  Result := 1;
end;

function CGregorianCalendar.IsLeapYear(year: Integer; era: Integer): boolean;
begin
  if ((era <> 0) and (era <> 1)) then
    raise ArgumentOutOfRangeException.Create('era', Environment.GetResourceString('ArgumentOutOfRange_InvalidEraValue'));
  if ((year < 1) or (year > $270f)) then
    raise ArgumentOutOfRangeException.Create('year', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_Range'), [1, $270f]));
  if ((year mod 4) <> 0) then
    begin
      Result := false;
      exit
    end;
  begin
    Result := (((year mod 100) <> 0) or ((year mod 400) = 0));
    exit
  end
end;

function CGregorianCalendar.ToDateTime(
  year: Integer;
  month: Integer;
  day: Integer;
  hour: Integer;
  minute: Integer;
  second: Integer;
  millisecond: Integer;
  era: Integer): CDateTime;
begin
  if ((era <> 0) and (era <> 1)) then
    raise ArgumentOutOfRangeException.Create('era', Environment.GetResourceString('ArgumentOutOfRange_InvalidEraValue'));
  begin
    Result := CDateTime.Create(year, month, day, hour, minute, second, millisecond);
    exit
  end
end;


{ CDateTimeFormatInfo }
constructor CDateTimeFormatInfo.Create;
begin
  self._firstDayOfWeek := -1;
  self._calendarWeekRule := -1;

  // kv: May 4, 2010, removed 'not'
  // self.formatFlags := not DateTimeFormatFlags.None;
  self.formatFlags := DateTimeFormatFlags.None;

  self.m_cultureTableRecord := (CCultureInfo.InvariantCulture.GetObject as CCultureInfo).m_cultureTableRecord;
  self.m_isDefaultCalendar := true;
  self.calendar := CGregorianCalendar.GetDefaultInstance;
  self.InitializeOverridableProperties
end;

constructor CDateTimeFormatInfo.Create(const cultureTable: CultureTableRecord; cultureID: Integer; const cal: Calendar);
begin
  self._firstDayOfWeek := -1;
  self._calendarWeekRule := -1;

  // kv: May 4, 2010, removed 'not'
  // self.formatFlags := not DateTimeFormatFlags.None;
  self.formatFlags := DateTimeFormatFlags.None;

  self.m_cultureTableRecord := cultureTable;
  self.Calendar := cal;

  // KV: Lines added.
  // I can't find how .Net initializes local date time strings without
  // calling InitializeOverridableProperties
  self.m_isDefaultCalendar := true;
  self.InitializeOverridableProperties;
end;

class function CDateTimeFormatInfo.GetInstance(const provider: IFormatProvider): DateTimeFormatInfo;
var
  dateTimeInfo: DateTimeFormatInfo;
  info2: TObject;
begin
  if provider <> nil then
    info2 := provider.GetObject else
    info2 := nil;

  if (info2 is CCultureInfo) and not CCultureInfo(info2).m_isInherited then
  begin
    dateTimeInfo := CCultureInfo(info2).dateTimeInfo;
    if (dateTimeInfo <> nil) then
      begin
        Result := dateTimeInfo;
        exit
      end;
    begin
      Result := CCultureInfo(info2).DateTimeFormat;
      exit
    end
  end;

  if Interfaces.Supports(provider, DateTimeFormatInfo, dateTimeInfo) then
  begin
    Result := dateTimeInfo;
    exit
  end;

  if (provider <> nil) then
  begin
    if Interfaces.Supports(provider.GetFormat(Global.GetTypeOf<DateTimeFormatInfo>), DateTimeFormatInfo, dateTimeInfo) then
    begin
      Result := dateTimeInfo;
      exit
    end
  end;

  begin
    Result := CDateTimeFormatInfo.CurrentInfo;
    exit
  end
end;

function CDateTimeFormatInfo.get_AMDesignator: CString;
begin
  Result := self._amDesignator;
end;

procedure CDateTimeFormatInfo.set_AMDesignator(const Value: CString);
begin

end;

function CDateTimeFormatInfo.get_Calendar: Calendar;
begin
  Result := self._calendar;
end;

procedure CDateTimeFormatInfo.set_Calendar(const Value: Calendar);
begin
  self._calendar := Value;
end;

function  CDateTimeFormatInfo.get_CalendarWeekRule: CalendarWeekRule;
begin
  Result := _CalendarWeekRule;
end;

procedure CDateTimeFormatInfo.set_CalendarWeekRule(const Value: CalendarWeekRule);
begin
  _CalendarWeekRule := Value;
end;

function CDateTimeFormatInfo.get_CompareInfo: CompareInfo;
begin
  if (self.m_compareInfo = nil) then
    if (CCultureTableRecord.IsCustomCultureId(self.CultureId)) then
      self.m_compareInfo := CCompareInfo.GetCompareInfo(self.m_cultureTableRecord.ICOMPAREINFO)
    else
      self.m_compareInfo := CCompareInfo.GetCompareInfo(self.CultureId);
    begin
      Result := self.m_compareInfo;
      exit
    end
end;

function  CDateTimeFormatInfo.get_DayNames : StringArray;
begin
  Result := self.GetDayOfWeekNames;
end;

procedure CDateTimeFormatInfo.set_DayNames(const Value: StringArray);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_Array'));
  if (Length(value) <> 7) then
    raise ArgumentException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Argument_InvalidArrayLength'), [7]), 'value');
//  self.CheckNullValue(value, value.Length);
  self.ClearTokenHashTable(true);
  self._dayNames := value
end;

function CDateTimeFormatInfo.get_CultureId: Integer;
begin
  Result := self.m_cultureTableRecord.CultureID
end;

function CDateTimeFormatInfo.get_FormatFlags: DateTimeFormatFlags;
begin
  Result := DateTimeFormatFlags.UseGenitiveMonth;
end;

function CDateTimeFormatInfo.get_FullDateTimePattern: CString;
begin
  if (self._fullDateTimePattern = nil) then
    self._fullDateTimePattern := CString.Concat(self.LongDatePattern, ' ', self.LongTimePattern);
  begin
    Result := self._fullDateTimePattern;
    exit
  end
end;

procedure CDateTimeFormatInfo.set_FullDateTimePattern(const Value: CString);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_String'));
  self._fullDateTimePattern := value
end;

function CDateTimeFormatInfo.get_GeneralLongTimePattern: CString;
begin
 if (self._generalLongTimePattern = nil) then
    self._generalLongTimePattern := CString.Concat(self.ShortDatePattern, ' ', self.LongTimePattern);
  begin
    Result := self._generalLongTimePattern;
    exit
  end
end;

function CDateTimeFormatInfo.get_GeneralShortTimePattern: CString;
begin
  if (self._generalShortTimePattern = nil) then
    self._generalShortTimePattern := CString.Concat(self.ShortDatePattern, ' ', self.ShortTimePattern);
  begin
    Result := self._generalShortTimePattern;
    exit
  end
end;

function CDateTimeFormatInfo.get_DateSeparator: CString;
begin
 if (self._dateSeparator = nil) then
    self._dateSeparator := self.m_cultureTableRecord.SDATE;
  begin
    Result := self._dateSeparator;
    exit
  end
end;

procedure CDateTimeFormatInfo.set_DateSeparator(const Value: CString);
begin

end;

function CDateTimeFormatInfo.get_DateTimeOffsetPattern: CString;
begin
 if (self._dateTimeOffsetPattern = nil) then
    self._dateTimeOffsetPattern := CString.Concat(self.ShortDatePattern, ' ', self.LongTimePattern, ' zzz');
  begin
    Result := self._dateTimeOffsetPattern;
    exit
  end
end;

function CDateTimeFormatInfo.get_EraNames: StringArray;
begin
  if (self.m_eraNames = nil) then
    if (self.Calendar.ID = 1) then
    begin
      SetLength(self.m_eraNames, 1);
      self.m_eraNames[0] := self.m_cultureTableRecord.SADERA;
    end
    else
      if (self.Calendar.ID <> 4) then
        self.m_eraNames := CalendarTable.Default.SERANAMES(self.Calendar.ID)
      else
      begin
        SetLength(self.m_eraNames, 1);
        self.m_eraNames [0] := CalendarTable.nativeGetEraName($404, self.Calendar.ID);
      end;
      begin
        Result := self.m_eraNames;
        exit
      end
end;

function CDateTimeFormatInfo.get_HasForceTwoDigitYears: Boolean;
begin
 case self.calendar.ID of
    3,
    4:
      begin
        begin
          Result := true;
          exit
        end
      end;
  end;
  begin
    Result := false;
    exit
  end
end;

function CDateTimeFormatInfo.get_HasSpacesInDayNames: Boolean;
begin
  Result := ((self.FormatFlags and DateTimeFormatFlags.UseSpacesInDayNames) <> DateTimeFormatFlags.None)
end;

function CDateTimeFormatInfo.get_HasYearMonthAdjustment: Boolean;
begin
  Result := ((self.FormatFlags and DateTimeFormatFlags.UseHebrewRule) <> DateTimeFormatFlags.None)
end;

function CDateTimeFormatInfo.get_LongDatePattern: CString;
begin
  Result := self._longDatePattern;
end;

procedure CDateTimeFormatInfo.set_LongDatePattern(const Value: CString);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_String'));
  self.ClearTokenHashTable(true);
  self.SetDefaultPatternAsFirstItem(self.allLongDatePatterns, value);
  self._longDatePattern := value;
  self._fullDateTimePattern := nil
end;

function CDateTimeFormatInfo.get_LongTimePattern: CString;
begin
  Result := self._longTimePattern;
end;

procedure CDateTimeFormatInfo.set_LongTimePattern(const Value: CString);
begin

end;

function CDateTimeFormatInfo.get_MonthDayPattern: CString;
var
  sMONTHDAY: CString;
begin
  if (self._monthDayPattern = nil) then
  begin
    if (self.m_isDefaultCalendar) then
      sMONTHDAY := self.m_cultureTableRecord.SMONTHDAY
    else
    begin
      sMONTHDAY := CalendarTable.Default.SMONTHDAY(self.Calendar.ID);
      if (sMONTHDAY.Length = 0) then
        sMONTHDAY := self.m_cultureTableRecord.SMONTHDAY
    end;
    self._monthDayPattern := sMONTHDAY
  end;
  begin
    Result := self._monthDayPattern;
    exit
  end
end;

procedure CDateTimeFormatInfo.set_MonthDayPattern(const Value: CString);
begin

end;

function  CDateTimeFormatInfo.get_MonthNames : StringArray;
begin
  Result := self.GetMonthNames;
end;

procedure CDateTimeFormatInfo.set_MonthNames(const Value: StringArray);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_Array'));
  if (Length(value) <> 13) then
    raise ArgumentException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Argument_InvalidArrayLength'), [13]), 'value');
//  self.CheckNullValue(value, (value.Length - 1));
  self._monthNames := value;
  self.ClearTokenHashTable(true)
end;

function CDateTimeFormatInfo.get_PMDesignator: CString;
begin
  Result := self._pmDesignator
end;

procedure CDateTimeFormatInfo.set_PMDesignator(const Value: CString);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_String'));
  self.ClearTokenHashTable(true);
  self._pmDesignator := value
end;

function CDateTimeFormatInfo.get_RFC1123Pattern: CString;
begin
  Result := 'ddd, dd MMM yyyy HH'':''mm'':''ss ''GMT'''
end;

function CDateTimeFormatInfo.get_ShortDatePattern: CString;
begin
  Result := self._shortDatePattern;
end;

procedure CDateTimeFormatInfo.set_ShortDatePattern(const Value: CString);
begin

end;

function CDateTimeFormatInfo.get_ShortTimePattern: CString;
begin
 if (self._shortTimePattern = nil) then
    self._shortTimePattern := self.m_cultureTableRecord.SSHORTTIME;
  begin
    Result := self._shortTimePattern;
    exit
  end
end;

procedure CDateTimeFormatInfo.set_ShortTimePattern(const Value: CString);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_String'));
  self._shortTimePattern := value;
  self._generalShortTimePattern := nil
end;

function CDateTimeFormatInfo.get_SortableDateTimePattern: CString;
begin
  Result := 'yyyy''-''MM''-''dd''T''HH'':''mm'':''ss';
end;

function CDateTimeFormatInfo.get_TimeSeparator: CString;
begin
 if (self._timeSeparator = nil) then
    self._timeSeparator := self.m_cultureTableRecord.STIME;
  begin
    Result := self._timeSeparator;
    exit
  end
end;

procedure CDateTimeFormatInfo.set_TimeSeparator(const Value: CString);
begin

end;

function CDateTimeFormatInfo.get_UniversalSortableDateTimePattern: CString;
begin
  Result := 'yyyy''-''MM''-''dd HH'':''mm'':''ss''Z'''
end;

function CDateTimeFormatInfo.get_YearMonthPattern: CString;
begin
  Result := self._yearMonthPattern;
end;

procedure CDateTimeFormatInfo.set_YearMonthPattern(const Value: CString);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_String'));
  self._yearMonthPattern := value;
//  self.SetDefaultPatternAsFirstItem(self.allYearMonthPatterns, self._yearMonthPattern);
end;

procedure CDateTimeFormatInfo.ClearTokenHashTable(scanDateWords: boolean);
begin
//  self.m_dtfiTokenHash := nil;
//  self.m_dateWords := nil;
//  self.m_scanDateWords := scanDateWords;
  self.formatFlags := not DateTimeFormatFlags.None
end;

function CDateTimeFormatInfo.Clone: CObject;
var
  intf: DateTimeFormatInfo;
  info: CDateTimeFormatInfo;
begin
  info := CDateTimeFormatInfo.Create;
  intf := info;

  info.allLongDatePatterns := allLongDatePatterns;
  info._abbreviatedDayNames := _abbreviatedDayNames;
  info._abbreviatedMonthNames := _abbreviatedMonthNames;
  info._calendarWeekRule := _calendarWeekRule;
  info._amDesignator := _amDesignator;
  info._calendar := _calendar;
  info._dateSeparator := _dateSeparator;
  info._dateTimeOffsetPattern := _dateTimeOffsetPattern;
  info._fullDateTimePattern := _fullDateTimePattern;
  info._longDatePattern := _longDatePattern;
  info._longTimePattern := _longTimePattern;
  info._monthDayPattern := _monthDayPattern;
  info._pmDesignator := _pmDesignator;
  info._generalLongTimePattern := _generalLongTimePattern;
  info._generalShortTimePattern := _generalShortTimePattern;
  info._shortDatePattern := _shortDatePattern;
  info._shortTimePattern := _shortTimePattern;
  info._timeSeparator := _timeSeparator;
  info._firstDayOfWeek := _firstDayOfWeek;
  info._yearMonthPattern := _yearMonthPattern;
  info.formatFlags := formatFlags;
  info._dayNames := _dayNames;
  info._monthNames := _monthNames;
  info.m_eraNames := m_eraNames;
  info.m_isDefaultCalendar := m_isDefaultCalendar;
  info.m_compareInfo := m_compareInfo;
  info.m_cultureTableRecord := m_cultureTableRecord;
  info.m_isReadOnly := m_isReadOnly;

  Result := intf;
end;

function CDateTimeFormatInfo.GetAbbreviatedDayOfWeekNames: StringArray;
var
  sABBREVDAYNAMES: StringArray;
begin
  if (self._abbreviatedDayNames = nil) then
  begin
    sABBREVDAYNAMES := nil;
    if (not self.m_isDefaultCalendar) then
      sABBREVDAYNAMES := CalendarTable.Default.SABBREVDAYNAMES(self.Calendar.ID);
    if (((sABBREVDAYNAMES = nil) or (Length(sABBREVDAYNAMES) = 0)) or (sABBREVDAYNAMES[0].Length = 0)) then
      sABBREVDAYNAMES := self.m_cultureTableRecord.SABBREVDAYNAMES;

    self._abbreviatedDayNames := sABBREVDAYNAMES
  end;
  begin
    Result := self._abbreviatedDayNames;
    exit
  end
end;

function CDateTimeFormatInfo.GetAbbreviatedDayName(dayofweek: DayOfWeek): CString;
begin
  if ((dayofweek < DayOfWeek.Sunday) or (dayofweek > DayOfWeek.Saturday)) then
    raise ArgumentOutOfRangeException.Create('dayofweek', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_Range'), [DayOfWeek.Sunday, DayOfWeek.Saturday]));
  begin
    if self._abbreviatedDayNames = nil then
      self.GetAbbreviatedDayOfWeekNames;

    Result := self._abbreviatedDayNames[Integer(dayofweek)];
    exit
  end
end;

function CDateTimeFormatInfo.GetAbbreviatedMonthName(month: Integer): CString;
begin
 if ((month < 1) or (month > 13)) then
    raise ArgumentOutOfRangeException.Create('month', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_Range'), [1, 13]));
  begin
    if self._abbreviatedMonthNames = nil then
      self.GetAbbreviatedMonthNames;

    Result := self._abbreviatedMonthNames[(month - 1)];
    exit
  end
end;

function CDateTimeFormatInfo.GetAbbreviatedMonthNames: StringArray;
var
  sABBREVMONTHNAMES: StringArray;
begin
 if (self._abbreviatedMonthNames = nil) then
  begin
    sABBREVMONTHNAMES := nil;
    if (not self.m_isDefaultCalendar) then
      sABBREVMONTHNAMES := CalendarTable.Default.SABBREVMONTHNAMES(self.Calendar.ID);
    if (((sABBREVMONTHNAMES = nil) or (Length(sABBREVMONTHNAMES) = 0)) or (sABBREVMONTHNAMES[0].Length = 0)) then
      sABBREVMONTHNAMES := self.m_cultureTableRecord.SABBREVMONTHNAMES;
    self._abbreviatedMonthNames := sABBREVMONTHNAMES
  end;
  begin
    Result := self._abbreviatedMonthNames;
    exit
  end
end;

function CDateTimeFormatInfo.GetDayOfWeekNames: StringArray;
var
  sDAYNAMES: StringArray;
begin
  if (self._dayNames = nil) then
  begin
    sDAYNAMES := nil;
    if (not self.m_isDefaultCalendar) then
      sDAYNAMES := CalendarTable.Default.SDAYNAMES(self.Calendar.ID);
    if (((sDAYNAMES = nil) or (Length(sDAYNAMES) = 0)) or (sDAYNAMES[0].Length = 0)) then
      sDAYNAMES := self.m_cultureTableRecord.SDAYNAMES;

    self._dayNames := sDAYNAMES
  end;
  begin
    Result := self._dayNames;
    exit
  end
end;

function CDateTimeFormatInfo.GetDayName(dayofweek: DayOfWeek): CString;
begin
  if ((dayofweek < DayOfWeek.Sunday) or (dayofweek > DayOfWeek.Saturday)) then
    raise ArgumentOutOfRangeException.Create('dayofweek', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_Range'), [DayOfWeek.Sunday, DayOfWeek.Saturday]));
  begin
    if _dayNames = nil then
      self.GetDayOfWeekNames;

    Result := _dayNames[Integer(dayofweek)];
    exit
  end
end;

function CDateTimeFormatInfo.GetEraName(era: Integer): CString;
begin
  if (era = 0) then
    era := self.Calendar.CurrentEraValue;

  dec(era);
  if ((era >= Length(self.EraNames)) or (era < 0)) then
    raise ArgumentOutOfRangeException.Create('era', Environment.GetResourceString('ArgumentOutOfRange_InvalidEraValue'));
  begin
    Result := self.m_eraNames[era];
    exit
  end
end;

function CDateTimeFormatInfo.GetMonthName(month: Integer): CString;
begin
  if ((month < 1) or (month > 13)) then
    raise ArgumentOutOfRangeException.Create('month', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_Range'), [1, 13] ));

  begin
    if self._monthNames = nil then
      self.GetMonthNames;

    Result := self._monthNames[(month - 1)];
    exit
  end
end;

function CDateTimeFormatInfo.GetMonthNames: StringArray;
var
  sMONTHNAMES: StringArray;
begin
  if (self._monthNames = nil) then
  begin
    sMONTHNAMES := nil;
    if (not self.m_isDefaultCalendar) then
      sMONTHNAMES := CalendarTable.Default.SMONTHNAMES(self.Calendar.ID);
    if (((sMONTHNAMES = nil) or (Length(sMONTHNAMES) = 0)) or (sMONTHNAMES[0].Length = 0)) then
      sMONTHNAMES := self.m_cultureTableRecord.SMONTHNAMES;

    self._monthNames := sMONTHNAMES
  end;
  begin
    Result := self._monthNames;
    exit
  end
end;

function CDateTimeFormatInfo.GetLongDatePattern(calID: Integer): CString;
begin
  if (not self.m_isDefaultCalendar) then
    begin
      Result := CalendarTable.Default.SLONGDATE(calID)[0];
      exit
    end;
  begin
    Result := self.m_cultureTableRecord.SLONGDATE;
    exit
  end
end;

function CDateTimeFormatInfo.GetShortDatePattern(calID: Integer): CString;
begin
  if (not self.m_isDefaultCalendar) then
    begin
      Result := CalendarTable.Default.SSHORTDATE(calID)[0];
      exit
    end;
  begin
    Result := self.m_cultureTableRecord.SSHORTDATE;
    exit
  end
end;

function CDateTimeFormatInfo.GetYearMonthPattern(calID: Integer): CString;
begin
  if (not self.m_isDefaultCalendar) then
    begin
      Result := CalendarTable.Default.SYEARMONTH(calID)[0];
      exit
    end;
  begin
//    Result := self.m_cultureTableRecord.SYEARMONTHS[0];
    Result := self.m_cultureTableRecord.SYEARMONTH;
    exit
  end
end;

function CDateTimeFormatInfo.internalGetMonthName(month: Integer; style: MonthNameStyles; abbreviated: boolean): CString;
var
  strArray: StringArray;
begin
  strArray := nil;
  case Integer(style) of
    MonthNameStyles.Genitive:
      strArray := self.internalGetGenitiveMonthNames(abbreviated);
    MonthNameStyles.LeapYear:
      strArray := self.internalGetLeapYearMonthNames;
  else
    if abbreviated then
      strArray := self.GetAbbreviatedMonthNames else
      strArray := self.GetMonthNames;
  end;

  if ((month < 1) or (month > Length(strArray))) then
    raise ArgumentOutOfRangeException.Create('month', CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('ArgumentOutOfRange_Range'), [1, Length(strArray)]));
  begin
    Result := strArray[(month - 1)];
    exit
  end
end;

function CDateTimeFormatInfo.internalGetGenitiveMonthNames(abbreviated: boolean): StringArray;
var
  sABBREVMONTHGENITIVENAMES: StringArray;
  sMONTHGENITIVENAMES: StringArray;
begin
  if (abbreviated) then
  begin
    if (self.m_genitiveAbbreviatedMonthNames = nil) then
    begin
      if (self.m_isDefaultCalendar) then
      begin
        sABBREVMONTHGENITIVENAMES := self.m_cultureTableRecord.SABBREVMONTHGENITIVENAMES;
        if (Length(sABBREVMONTHGENITIVENAMES) > 0) then
          self.m_genitiveAbbreviatedMonthNames := sABBREVMONTHGENITIVENAMES
        else
          self.m_genitiveAbbreviatedMonthNames := self.GetAbbreviatedMonthNames
        end
      else
        self.m_genitiveAbbreviatedMonthNames := self.GetAbbreviatedMonthNames;
    end;

    begin
      Result := self.m_genitiveAbbreviatedMonthNames;
      exit
    end
  end;

  if (self._genitiveMonthNames = nil) then
  begin
    if (self.m_isDefaultCalendar) then
    begin
      sMONTHGENITIVENAMES := self.m_cultureTableRecord.SMONTHGENITIVENAMES;
      if (Length(sMONTHGENITIVENAMES) > 0) then
        self._genitiveMonthNames := sMONTHGENITIVENAMES
      else
        self._genitiveMonthNames := self.GetMonthNames
      end
    else
      self._genitiveMonthNames := self.GetMonthNames;
  end;
  begin
    Result := self._genitiveMonthNames;
    exit
  end
end;

function CDateTimeFormatInfo.internalGetLeapYearMonthNames: StringArray;
var
  strArray: StringArray;
begin
  if (self._leapYearMonthNames = nil) then
    if (self.m_isDefaultCalendar) then
      self._leapYearMonthNames := self.GetMonthNames
    else
    begin
      strArray := CalendarTable.Default.SLEAPYEARMONTHNAMES(self.Calendar.ID);
      if (Length(strArray) > 0) then
        self._leapYearMonthNames := strArray
      else
        self._leapYearMonthNames := self.GetMonthNames
    end;
  begin
    Result := self._leapYearMonthNames;
    exit
  end
end;

procedure CDateTimeFormatInfo.InitializeOverridableProperties;
begin
  if (self._amDesignator = nil) then
    self._amDesignator := self.m_cultureTableRecord.S1159;
  if (self._pmDesignator = nil) then
    self._pmDesignator := self.m_cultureTableRecord.S2359;
  if (self._longTimePattern = nil) then
    self._longTimePattern := self.m_cultureTableRecord.STIMEFORMAT;
  if (self._firstDayOfWeek = -1) then
    self._firstDayOfWeek := self.m_cultureTableRecord.IFIRSTDAYOFWEEK;
  if (self._calendarWeekRule = -1) then
    self._calendarWeekRule := self.m_cultureTableRecord.IFIRSTWEEKOFYEAR;
  if (self._yearMonthPattern = nil) then
    self._yearMonthPattern := self.GetYearMonthPattern(self.calendar.ID);
  if (self._shortDatePattern = nil) then
    self._shortDatePattern := self.GetShortDatePattern(self.calendar.ID);
  if (self._longDatePattern = nil) then
    self._longDatePattern := self.GetLongDatePattern(self.calendar.ID)
end;

procedure CDateTimeFormatInfo.SetDefaultPatternAsFirstItem(
  const patterns: StringArray;
  const defaultPattern: CString);
var
  i: Integer;
  j: Integer;
  str: CString;
begin
  if (patterns <> nil) then
  begin
    i := 0;

    while ((i < Length(patterns))) do
    begin
      if (patterns[i].Equals(defaultPattern)) then
      begin
        if (i <> 0) then
        begin
          str := patterns[i];
          j := i;
          while ((j > 0)) do
          begin
            patterns[j] := patterns[(j - 1)];
            dec(j)
          end;
          patterns[0] := str
        end;
        exit
      end;
      inc(i)
    end;
  end;
end;

procedure CDateTimeFormatInfo.VerifyWritable;
begin
  if (self.m_isReadOnly) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_ReadOnly'))
end;

function CDateTimeFormatInfo.YearMonthAdjustment(var year: Integer; var month: Integer; parsedMonthName: boolean): boolean;
begin
  if ((self.FormatFlags and DateTimeFormatFlags.UseHebrewRule) <> DateTimeFormatFlags.None) then
  begin
    if (year < $3e8) then
      inc(year, $1388);

    if ((year < self.Calendar.GetYear(self.Calendar.MinSupportedDateTime)) or (year > self.Calendar.GetYear(self.Calendar.MaxSupportedDateTime))) then
    begin
      Result := false;
      exit
    end;

    if (parsedMonthName and not self.Calendar.IsLeapYear(year)) then
    begin
      if (month >= 8) then
        dec(month)
      else if (month = 7) then
      begin
        Result := false;
        exit
      end;
    end;
  end;

  begin
    Result := true;
    exit
  end
end;

function CDateTimeFormatInfo.GetFormat(AClass: TClass): IBaseInterface;
begin
  raise NotImplementedException.Create;
end;

function CDateTimeFormatInfo.GetFormat(const AType: &Type): IBaseInterface;
begin
  raise NotImplementedException.Create;
end;

class function CDateTimeFormatInfo.get_CurrentInfo: DateTimeFormatInfo;
var
  currentCulture: CCultureInfo;
  dateTimeInfo: DateTimeFormatInfo;
begin
  currentCulture := Thread.CurrentThread.CurrentCulture.GetObject as CCultureInfo;
  if (not currentCulture.m_isInherited) then
  begin
    dateTimeInfo := currentCulture.dateTimeInfo;
    if (dateTimeInfo <> nil) then
      begin
        Result := dateTimeInfo;
        exit
      end
    end;
  begin
    Result := (currentCulture.GetFormat(Global.GetTypeOf<DateTimeFormatInfo>) as DateTimeFormatInfo);
    exit
  end


//  if _CurrentInfo = nil then
//  begin
//    Lock(nil);
//    if _CurrentInfo = nil then
//      _CurrentInfo := CDateTimeFormatInfo.Create;
//  end;
//
//  Result := _CurrentInfo;
end;

class function CDateTimeFormatInfo.get_InvariantInfo: DateTimeFormatInfo;
begin
  if _InvariantInfo = nil then
  begin
    Lock(nil);
    if _InvariantInfo = nil then
      _InvariantInfo := CDateTimeFormatInfo.Create;
  end;

  Result := _InvariantInfo;
end;

{ DateTimeFormat }
class procedure DateTimeFormat._Create;
begin
  DateTimeFormat.NullOffset := CTimeSpan.MinValue;

  SetLength(DateTimeFormat.allStandardFormats, $13);
  DateTimeFormat.allStandardFormats[0] := 'd';
  DateTimeFormat.allStandardFormats[1] := 'D';
  DateTimeFormat.allStandardFormats[2] := 'f';
  DateTimeFormat.allStandardFormats[3] := 'F';
  DateTimeFormat.allStandardFormats[4] := 'g';
  DateTimeFormat.allStandardFormats[5] := 'G';
  DateTimeFormat.allStandardFormats[6] := 'm';
  DateTimeFormat.allStandardFormats[7] := 'M';
  DateTimeFormat.allStandardFormats[8] := 'o';
  DateTimeFormat.allStandardFormats[9] := 'O';
  DateTimeFormat.allStandardFormats[10] := 'r';
  DateTimeFormat.allStandardFormats[11] := 'R';
  DateTimeFormat.allStandardFormats[12] := 's';
  DateTimeFormat.allStandardFormats[13] := 't';
  DateTimeFormat.allStandardFormats[14] := 'T';
  DateTimeFormat.allStandardFormats[15] := 'u';
  DateTimeFormat.allStandardFormats[16] := 'U';
  DateTimeFormat.allStandardFormats[17] := 'y';
  DateTimeFormat.allStandardFormats[18] := 'Y';

  SetLength(DateTimeFormat.fixedNumberFormats, 7);
  DateTimeFormat.fixedNumberFormats[0] := '0';
  DateTimeFormat.fixedNumberFormats[1] := '00';
  DateTimeFormat.fixedNumberFormats[2] := '000';
  DateTimeFormat.fixedNumberFormats[3] := '0000';
  DateTimeFormat.fixedNumberFormats[4] := '00000';
  DateTimeFormat.fixedNumberFormats[5] := '000000';
  DateTimeFormat.fixedNumberFormats[6] := '0000000';
end;

class function DateTimeFormat.ExpandPredefinedFormat(
  const format: CString;
  var dateTime: CDateTime;
  var dtfi: DateTimeFormatInfo;
  var offset: CTimeSpan): CString;
label
  Label_015B;

begin
 case Char(format.Chars[0]) of
    'o',
    'O':
      begin
        dtfi := CDateTimeFormatInfo.InvariantInfo;
        goto Label_015B
      end;
    'r',
    'R':
      begin
        if (offset <> DateTimeFormat.NullOffset) then
          dateTime := dateTime.Subtract(offset)
        else
          if (dateTime.Kind = DateTimeKind.Local) then
            DateTimeFormat.InvalidFormatForLocal(format, dateTime);
          dtfi := CDateTimeFormatInfo.InvariantInfo;
          goto Label_015B
        end;
      's':
        begin
          dtfi := CDateTimeFormatInfo.InvariantInfo;
          goto Label_015B
        end;
      'u':
        begin
          if (offset = DateTimeFormat.NullOffset) then
          begin
            if (dateTime.Kind = DateTimeKind.Local) then
              DateTimeFormat.InvalidFormatForLocal(format, dateTime);
          end else
            dateTime := dateTime.Subtract(offset);
        end;
      'U':
        begin
          if (offset <> DateTimeFormat.NullOffset) then
            raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'));
          dtfi := (IBaseInterface(dtfi.Clone) as DateTimeFormatInfo);
          if (dtfi.Calendar.GetType <> Global.GetTypeOf(CGregorianCalendar)) then
            dtfi.Calendar := CGregorianCalendar.GetDefaultInstance;
          dateTime := dateTime.ToUniversalTime;
          goto Label_015B
        end;
    else
    begin
      goto Label_015B
    end;
  end;

  dtfi := CDateTimeFormatInfo.InvariantInfo;

  Label_015B:
  begin
    Result := DateTimeFormat.GetRealFormat(format, dtfi);
    Exit;
  end;

  Result := format;
end;

class function DateTimeFormat.Format(
  const DateTime: CDateTime;
  const format: CString;
  dtfi: DateTimeFormatInfo) : CString;
begin
  Result := DateTimeFormat.Format(DateTime, format, dtfi,  DateTimeFormat.NullOffset);
end;

class function DateTimeFormat.Format(
  {const} DateTime: CDateTime;
  {const} format: CString;
  {const} dtfi: DateTimeFormatInfo;
  {const} Offset: CTimeSpan) : CString;
var
  flag: Boolean;
begin
  if ((format = nil) or (format.Length = 0)) then
  begin
    flag := false;
    if (dateTime.Ticks < $c92a69c000) then
      case dtfi.Calendar.ID of
        3,
        4,
        6,
        8,
        13,
        $17:
        begin
          flag := true;
          dtfi := CDateTimeFormatInfo.InvariantInfo;
        end;
      end;

    if (offset = DateTimeFormat.NullOffset) then
    begin
      if (flag) then
        format := 's'
      else
        format := 'G';
    end
    else if (flag) then
      format := 'yyyy''-''MM''-''ddTHH'':''mm'':''ss zzz'
    else
      format := dtfi.DateTimeOffsetPattern
  end;

  if (format.Length = 1) then
    format := DateTimeFormat.ExpandPredefinedFormat(format, dateTime, dtfi, offset);
  begin
    Result := DateTimeFormat.FormatCustomized(dateTime, format, dtfi, offset);
    exit
  end
end;

class function DateTimeFormat.FormatCustomized(
  const dateTime: CDateTime;
  const format: CString;
  const dtfi: DateTimeFormatInfo;
  const offset: CTimeSpan): CString;
label
  Label_03D3, Label_03C7, Label_042E,
  Label_037F, Label_0373, Label_0399,
  Label_0327, Label_02FE,
  Label_0466, Label_04B5,
  Label_05A4;

var
  ch3: CChar;
  _calendar: Calendar;
  num2: Integer;
  num4: Integer;
  dayOfMonth: Integer;
  flag: Boolean;
  i: Integer;
  num8: Integer;
  month: Integer;
  num3: Integer;
  num5: Int64;
  num6: Integer;
  outputBuffer: StringBuilder;
  patternChar: CChar;
  str: CString;
  _result: StringBuilder;
  timeOnly: Boolean;
  year: Integer;
  {$IFDEF DELPHIXE104_UP}
  dummy: IBaseInterface;
  {$ENDIF}
begin
  month := 0;
  dayOfMonth := 0;
  year := 0;
  _calendar := dtfi.Calendar;
  outputBuffer := CStringBuilder.Create;
  flag := (_calendar.ID = 8);
  timeOnly := true;
  i := 0;
  num2 := 0;
  while ((i + num2) < format.Length) do
  begin
    inc(i, num2);
    patternChar := format.Chars[i];
    case SystemChar(patternChar) of
      'F',
      'f':
        begin

        end;
      'H':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          DateTimeFormat.FormatDigits(outputBuffer, dateTime.Hour, num2);
          continue;

        end;
      ':':
        begin
          outputBuffer.Append(dtfi.TimeSeparator);
          num2 := 1;
          continue;

        end;
      '/':
        begin
          outputBuffer.Append(dtfi.DateSeparator);
          num2 := 1;
          continue;

        end;
      '%':
        begin
          num4 := DateTimeFormat.ParseNextChar(format, i);
          if ((num4 < 0) or (num4 = $25)) then
            raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'));
          ch3 := SystemChar(num4);
          outputBuffer.Append(DateTimeFormat.FormatCustomized(dateTime, ch3.ToString, dtfi, offset));
          num2 := 2;
          continue;

        end;
      '''',
      '"':
        begin
          _result := CStringBuilder.Create;
          num2 := DateTimeFormat.ParseQuoteString(format, i, _result);
          outputBuffer.Append(_result);
          continue;

        end;
      'K':
        begin
          num2 := 1;
          DateTimeFormat.FormatCustomizedRoundripTimeZone(dateTime, offset, outputBuffer);
          continue;

        end;
      'M':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          month := _calendar.GetMonth(dateTime);
          if (num2 > 2) then
            goto Label_03D3;
          if (not flag) then
            goto Label_03C7;
          DateTimeFormat.HebrewFormatDigits(outputBuffer, month);
          goto Label_042E
        end;
      '\':
        begin
          num4 := DateTimeFormat.ParseNextChar(format, i);
          if (num4 < 0) then
            raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'));
          outputBuffer.Append(SystemChar(num4));
          num2 := 2;
          continue;

        end;
      'd':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          if (num2 > 2) then
            goto Label_037F;
          dayOfMonth := _calendar.GetDayOfMonth(dateTime);
          if (not flag) then
            goto Label_0373;
          DateTimeFormat.HebrewFormatDigits(outputBuffer, dayOfMonth);
          goto Label_0399
        end;
      'g':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          outputBuffer.Append(dtfi.GetEraName(_calendar.GetEra(dateTime)));
          continue;

        end;
      'h':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          num3 := (dateTime.Hour mod 12);
          if (num3 = 0) then
            num3 := 12;
          DateTimeFormat.FormatDigits(outputBuffer, num3, num2);
          continue;

        end;
      's':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          DateTimeFormat.FormatDigits(outputBuffer, dateTime.Second, num2);
          continue;

        end;
      't':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          if (num2 <> 1) then
            goto Label_0327;
          if (dateTime.Hour >= 12) then
            goto Label_02FE;
          if (dtfi.AMDesignator.Length >= 1) then
            outputBuffer.Append(dtfi.AMDesignator.Chars[0]);
          continue;

        end;
      'm':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          DateTimeFormat.FormatDigits(outputBuffer, dateTime.Minute, num2);
          continue;

        end;
      'y':
        begin
          year := _calendar.GetYear(dateTime);
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          if (not dtfi.HasForceTwoDigitYears) then
            goto Label_0466;

          if (num2 <= 2) then
            DateTimeFormat.FormatDigits(outputBuffer, year,  num2) else
            DateTimeFormat.FormatDigits(outputBuffer, year,  2);
          goto Label_04B5
        end;
      'z':
        begin
          num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
          DateTimeFormat.FormatCustomizedTimeZone(dateTime, offset, format, num2, timeOnly, outputBuffer);
          continue;

        end;
    else
      goto Label_05A4
    end;

    num2 := DateTimeFormat.ParseRepeatPattern(format, i, patternChar);
    if (num2 > 7) then
      raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'));

    num5 := (dateTime.Ticks mod $989680);
    num5 := (num5 div CMath.Truncate(CMath.Pow(10, ((7 - num2)))));
    if (patternChar = 'f') then
    begin
      outputBuffer.Append(CInteger(num5).ToString(DateTimeFormat.fixedNumberFormats[(num2 - 1)], CCultureInfo.InvariantCulture));
      continue;

    end;
    num6 := num2;
    while ((num6 > 0)) do
    begin
      if ((num5 mod 10) <> 0) then
        break;
        ;
      num5 := (num5 div 10);
      dec(num6)
    end;

    if (num6 > 0) then
      {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(CInteger(num5).ToString(DateTimeFormat.fixedNumberFormats[(num6 - 1)], CCultureInfo.InvariantCulture))
    else
      if ((outputBuffer.Length > 0) and (outputBuffer.Chars[(outputBuffer.Length - 1)] = '.')) then
        {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Remove((outputBuffer.Length - 1), 1);
      continue;
      ;

    Label_02FE:
      if (dtfi.PMDesignator.Length >= 1) then
        {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(dtfi.PMDesignator.Chars[0]);
      continue;
      ;

    Label_0327:
      if (dateTime.Hour < 12)  then
        {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append( dtfi.AMDesignator) else
        {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append( dtfi.PMDesignator);
      continue;
      ;

    Label_0373:
      DateTimeFormat.FormatDigits(outputBuffer, dayOfMonth, num2);
      goto Label_0399;

    Label_037F:
      num8 := Integer(_calendar.GetDayOfWeek(dateTime));
      {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(DateTimeFormat.FormatDayOfWeek(num8, num2, dtfi));

    Label_0399:
      timeOnly := false;
      continue;
      ;

    Label_03C7:
      DateTimeFormat.FormatDigits(outputBuffer, month, num2);
      goto Label_042E;

    Label_03D3:
      if (flag) then
        {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(DateTimeFormat.FormatHebrewMonthName(dateTime, month, num2, dtfi))
      else
        if (((dtfi.FormatFlags and DateTimeFormatFlags.UseGenitiveMonth) <> DateTimeFormatFlags.None) and (num2 >= 4)) then
        begin
          if DateTimeFormat.IsUseGenitiveForm(format, i, num2, 'd') then
            {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(dtfi.internalGetMonthName(month,  MonthNameStyles.Genitive, false)) else
            {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(dtfi.internalGetMonthName(month,  MonthNameStyles.Regular, false));
        end else
          {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(DateTimeFormat.FormatMonth(month, num2, dtfi));

      Label_042E:
        timeOnly := false;
        continue;
        ;

      Label_0466:
        if (_calendar.ID = 8) then
          DateTimeFormat.HebrewFormatDigits(outputBuffer, year)
        else
          if (num2 <= 2) then
            DateTimeFormat.FormatDigits(outputBuffer, (year mod 100), num2)
          else
          begin
            str := CString.Concat(CChar('D').ToString, CInteger(num2).ToString);
            {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(CInteger(year).ToString(str, CCultureInfo.InvariantCulture));
          end;

      Label_04B5:
        timeOnly := false;
        continue;
        ;

      Label_05A4:
        {$IFDEF DELPHIXE104_UP}dummy := {$ENDIF}outputBuffer.Append(patternChar);
        num2 := 1;
//        inc(i, num2)
      end;
      begin
        Result := outputBuffer.ToString;
        exit
      end
    end;

class procedure DateTimeFormat.FormatCustomizedRoundripTimeZone(
  const dateTime: CDateTime;
  {const} offset: CTimeSpan;
  const _result: StringBuilder);
label
  Label_0042;

begin
  if (offset = DateTimeFormat.NullOffset) then
  begin
    case Integer(dateTime.Kind) of
      DateTimeKind.Utc:
        begin
          _result.Append('Z');
          exit
        end;
      DateTimeKind.Local:
        begin
          offset := TimeZone.CurrentTimeZone.GetUtcOffset(dateTime);
          goto Label_0042
        end;
    end;
    exit
  end;

Label_0042:
  if (offset >= CTimeSpan.Zero) then
    _result.Append('+')
  else
  begin
    _result.Append('-');
    offset := offset.Negate
  end;
  _result.AppendFormat(CCultureInfo.InvariantCulture, '{0:00}:{1:00}', [offset.Hours, offset.Minutes]);
end;

class procedure DateTimeFormat.FormatCustomizedTimeZone(
  {const} dateTime: CDateTime;
  {const} offset: CTimeSpan;
  const format: CString;
  tokenLen: Integer;
  timeOnly: boolean;
  const _result: StringBuilder);
begin
  if (offset = DateTimeFormat.NullOffset) then
    if (timeOnly and (dateTime.Ticks < $c92a69c000)) then
      offset := TimeZone.CurrentTimeZone.GetUtcOffset(DateTime.Now)
    else
    begin
      if (dateTime.Kind = DateTimeKind.Utc) then
      begin
        DateTimeFormat.InvalidFormatForUtc(format, dateTime);
        dateTime := CDateTime.SpecifyKind(dateTime, DateTimeKind.Local)
      end;
      offset := TimeZone.CurrentTimeZone.GetUtcOffset(dateTime)
    end;
  if (offset >= CTimeSpan.Zero) then
    _result.Append('+')
  else
  begin
    _result.Append('-');
    offset := offset.Negate
  end;
  if (tokenLen <= 1) then
    _result.AppendFormat(CCultureInfo.InvariantCulture, '{0:0}', [offset.Hours])
  else
  begin
    _result.AppendFormat(CCultureInfo.InvariantCulture, '{0:00}', [offset.Hours]);
    if (tokenLen >= 3) then
      _result.AppendFormat(CCultureInfo.InvariantCulture, ':{0:00}', [offset.Minutes])
  end
end;

class function DateTimeFormat.FormatDayOfWeek(
  _dayOfWeek: Integer;
  _repeat: Integer;
  const dtfi: DateTimeFormatInfo): CString;

begin
  if (_repeat = 3) then
    begin
      Result := dtfi.GetAbbreviatedDayName(DayOfWeek(_dayOfWeek));
      exit
    end;
  begin
    Result := dtfi.GetDayName(DayOfWeek(_dayOfWeek));
    exit
  end
end;

class procedure DateTimeFormat.FormatDigits(
  const outputBuffer: StringBuilder;
  value: Integer;
  len: Integer);
var
  chPtr: PSystemChar;
  chPtr2: PSystemChar;
  count: Integer;
  num: Integer;

begin
  if (len > 2) then
    len := 2;
  GetMem(chPtr, 2 * $10);
  try
    chPtr2 := Pointer(NativeUInt(chPtr) + $10);
    num := value;
    repeat
      dec(chPtr2);
      chPtr2^ := SystemChar(((num mod 10) + $30));
      num := (num div 10)
    until ((num = 0) or (NativeUInt(chPtr2) <= NativeUInt(chPtr)));
    count := ((NativeUInt(chPtr) + $10) - NativeUInt(chPtr2)) div 2;
    while (((count < len) and (NativeUInt(chPtr2) > NativeUInt(chPtr)))) do
    begin
      dec(chPtr2);
      chPtr2^ := '0';
      inc(count)
    end;
    outputBuffer.Append(chPtr2, count)
  finally
    FreeMem(chPtr);
  end;
end;

class function DateTimeFormat.FormatHebrewMonthName(
  const time: CDateTime;
  month: Integer;
  repeatCount: Integer;
  const dtfi: DateTimeFormatInfo): CString;

begin
  if (dtfi.Calendar.IsLeapYear(dtfi.Calendar.GetYear(time))) then
    begin
      Result := dtfi.internalGetMonthName(month, MonthNameStyles.LeapYear, (repeatCount = 3));
      exit
    end;
  if (month >= 7) then
    inc(month);
  if (repeatCount = 3) then
    begin
      Result := dtfi.GetAbbreviatedMonthName(month);
      exit
    end;
  begin
    Result := dtfi.GetMonthName(month);
    exit
  end
end;

class function DateTimeFormat.FormatMonth(
  month: Integer;
  repeatCount: Integer;
  const dtfi: DateTimeFormatInfo): CString;
begin
  if (repeatCount = 3) then
    begin
      Result := dtfi.GetAbbreviatedMonthName(month);
      exit
    end;
  begin
    Result := dtfi.GetMonthName(month);
    exit
  end
end;

class function DateTimeFormat.GetRealFormat(const format: CString; const dtfi: DateTimeFormatInfo): CString;
begin
  case Char(format.Chars[0]) of
    'D':
      begin
        begin
          Result := dtfi.LongDatePattern;
          exit
        end
      end;
    'F':
      begin
        begin
          Result := dtfi.FullDateTimePattern;
          exit
        end
      end;
    'G':
      begin
        begin
          Result := dtfi.GeneralLongTimePattern;
          exit
        end
      end;
    'M',
    'm':
      begin
        begin
          Result := dtfi.MonthDayPattern;
          exit
        end
      end;
    'O',
    'o':
      begin
        begin
          Result := 'yyyy''-''MM''-''dd''T''HH'':''mm'':''ss.fffffffK';
          exit
        end
      end;
    'R',
    'r':
      begin
        begin
          Result := dtfi.RFC1123Pattern;
          exit
        end
      end;
    'T':
      begin
        begin
          Result := dtfi.LongTimePattern;
          exit
        end
      end;
    'U':
      begin
        begin
          Result := dtfi.FullDateTimePattern;
          exit
        end
      end;
    'd':
      begin
        begin
          Result := dtfi.ShortDatePattern;
          exit
        end
      end;
    'f':
      begin
        begin
          Result := CString.Concat(dtfi.LongDatePattern, ' ', dtfi.ShortTimePattern);
          exit
        end
      end;
    'g':
      begin
        begin
          Result := dtfi.GeneralShortTimePattern;
          exit
        end
      end;
    'Y',
    'y':
      begin
        begin
          Result := dtfi.YearMonthPattern;
          exit
        end
      end;
    's':
      begin
        begin
          Result := dtfi.SortableDateTimePattern;
          exit
        end
      end;
    't':
      begin
        begin
          Result := dtfi.ShortTimePattern;
          exit
        end
      end;
    'u':
      begin
        begin
          Result := dtfi.UniversalSortableDateTimePattern;
          exit
        end
      end;
  end;
  raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'))
end;

class procedure DateTimeFormat.HebrewFormatDigits(
  const outputBuffer: StringBuilder;
  digits: Integer);
begin
  outputBuffer.Append(HebrewNumber.ToString(digits))
end;

class function DateTimeFormat.IsUseGenitiveForm(
  const format: CString;
  index: Integer;
  tokenLen: Integer;
  const patternToMatch: CChar): boolean;
var
  num: Integer;
  num2: Integer;
begin
  num2 := 0;
  num := (index - 1);
  while (((num >= 0) and (format.Chars[num] <> patternToMatch))) do
  begin
    dec(num)
  end;
  if (num >= 0) then
  begin
    dec(num);
    while (((num >= 0) and (format.Chars[num] = patternToMatch))) do
    begin
      inc(num2);
      dec(num);
    end;
    if (num2 <= 1) then
      begin
        Result := true;
        exit
      end
    end;
  num := (index + tokenLen);
  while (((num < format.Length) and (format.Chars[num] <> patternToMatch))) do
  begin
    inc(num)
  end;
  if (num < format.Length) then
  begin
    num2 := 0;
    inc(num);
    while (((num < format.Length) and (format.Chars[num] = patternToMatch))) do
    begin
      inc(num2);
      inc(num);
    end;
    if (num2 <= 1) then
      begin
        Result := true;
        exit
      end
    end;
  begin
    Result := false;
    exit
  end
end;

class procedure DateTimeFormat.InvalidFormatForLocal(
  const format: CString;
  const dateTime: CDateTime);
begin

end;

class procedure DateTimeFormat.InvalidFormatForUtc(
  const format: CString;
  const dateTime: CDateTime);
begin
  // .Net
  // Mda.DateTimeInvalidLocalFormat
  // .Net

  raise FormatException.Create(Environment.GetResourceString('Format_DateTimeInvalidLocalFormat'));
end;


class function DateTimeFormat.ParseNextChar(const format: CString; pos: Integer): Integer;
begin
 if (pos >= (format.Length - 1)) then
    begin
      Result := -1;
      exit
    end;
  begin
    Result := Integer(char(format.Chars[(pos + 1)]));
    exit
  end
end;

class function DateTimeFormat.ParseQuoteString(
  const format: CString;
  pos: Integer;
  const _result: StringBuilder): Integer;
var
  ch: CChar;
  ch2: CChar;
  flag: Boolean;
  length: Integer;
  num2: Integer;
begin
  length := format.Length;
  num2 := pos;
  ch := format.Chars[pos];
  inc(pos);
  flag := false;
  while ((pos < length)) do
  begin
    ch2 := format.Chars[pos];
    inc(pos);
    if (ch2 = ch) then
    begin
      flag := true;
      break;

    end;
    if (ch2 = '\') then
    begin
      if (pos >= length) then
        raise FormatException.Create(Environment.GetResourceString('Format_InvalidString'));
      _result.Append(format.Chars[pos]);
      inc(pos);
    end
    else
      _result.Append(ch2)
    end;
  if (not flag) then
    raise FormatException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Format_BadQuote'), [ch] ));
  begin
    Result := (pos - num2);
    exit
  end
end;

class function DateTimeFormat.ParseRepeatPattern(
  const format: CString;
  pos: Integer;
  const patternChar: CChar): Integer;
var
  length: Integer;
  num2: Integer;
begin
  length := format.Length;
  num2 := (pos + 1);
  while (((num2 < length) and (format.Chars[num2] = patternChar))) do
  begin
    inc(num2)
  end;
  begin
    Result := (num2 - pos);
    exit
  end
end;

{ CCompareInfo }

function CCompareInfo.Compare(const Str1, Str2: CString): Integer;
begin
  Result := CString.Compare(Str1, Str2);
end;

function CCompareInfo.Compare(
  const string1: CString;
  offset1: Integer;
  length1: Integer;
  const string2: CString;
  offset2: Integer;
  length2: Integer;
  options: CompareOptions): Integer;
begin
  Result := 0;
end;

class function CCompareInfo.GetCompareInfo(culture: Integer): CompareInfo;
begin
  Result := _classLock.GetObject as CCompareInfo;
end;

{ CCultureTableRecord }
constructor CCultureTableRecord.Create(cultureId: Integer; useUserOverride: boolean);
begin
  self.m_bUseUserOverride := useUserOverride;
  self.m_ActualCultureID := cultureId;
//  self.m_CultureTable := CultureTable.Default;
  if (cultureId = $40a) then
    self.m_CultureID :=  cultureId else
    self.m_CultureID :=  self.ILANGUAGE;

  self.m_CultureName := self.SNAME;

end;

constructor CCultureTableRecord.Create(const cultureName: CString; useUserOverride: boolean);
var
  dataItem: Integer;
  dataItemFromCultureID: Integer;
  name: CString;
  num3: Integer;
  num4: Integer;
  str: CString;
  str3: CString;
  str5: CString;
  table: CultureTable;
  _actualName: CString;
  _cultureID: Integer;
  _langID: Integer;
begin
//  _cultureID := 0;
  if (cultureName.Length = 0) then
  begin
    useUserOverride := false;
    _cultureID := $7f;
//
//    // Wine fix: Wine does not support LOCALE_INVARIANT
//    if not IsValidLocale(_cultureID, LCID_INSTALLED) then
//      _cultureID := 1033; // English US
  end else
    _cultureID := Integer(SyntheticNameToLcidCache[cultureName]);

  self.m_bUseUserOverride := useUserOverride;
  self.m_CultureID :=  _cultureID;
  self.m_ActualName := cultureName;
  self.m_CultureName := cultureName;

  Exit;

  dataItemFromCultureID := -1;
  if (cultureName.Length > 0) then
  begin
    name := cultureName;
    num4 := CCultureTable.Default.GetDataItemFromCultureName(name, num3, str);
    if ((num4 >= 0) and ((CCultureInfo.GetSortID(num3) > 0) or (num3 = $40a))) then
    begin
      if (num3 = $40a) then
        _langID := $c0a
      else
        _langID := CCultureInfo.GetLangID(num3);
      if (CCultureTable.Default.GetDataItemFromCultureID(_langID, str3) >= 0) then
        name := CCultureTableRecord.ValidateCulturePieceToLower(str3, 'cultureName', $54)
    end;

// .Net
//    if (not Environment.GetCompatibilityFlag(CompatibilityFlag.DisableReplacementCustomCulture) or CCultureTableRecord.IsCustomCultureId(num3)) then
//      self.m_CultureTable := self.GetCustomCultureTable(name);
//    if (self.m_CultureTable <> nil) then
//    begin
//      dataItemFromCultureID := self.m_CultureTable.GetDataItemFromCultureName(name, @(self.m_ActualCultureID), @(self.m_ActualName));
//      if (num4 >= 0) then
//      begin
//        self.m_ActualCultureID := num3;
//        self.m_ActualName := str
//      end
//    end;
// .Net

    if ((dataItemFromCultureID < 0) and (num4 >= 0)) then
    begin
      self.m_CultureTable := CCultureTable.Default;
      self.m_ActualCultureID := num3;
      self.m_ActualName := str;
      dataItemFromCultureID := num4
    end;

    if (dataItemFromCultureID < 0) then
    begin
      CCultureTableRecord.InitSyntheticMapping;
      if (CCultureTableRecord.SyntheticNameToLcidCache.Item[name] <> nil) then
        _cultureID := Integer(CCultureTableRecord.SyntheticNameToLcidCache.Item[name])
    end
  end;

  if ((dataItemFromCultureID < 0) and (_cultureID > 0)) then
  begin
    if (_cultureID = $7f) then
    begin
      dataItemFromCultureID := CCultureTable.Default.GetDataItemFromCultureID(_cultureID, self.m_ActualName);
      if (dataItemFromCultureID > 0) then
      begin
        self.m_ActualCultureID := cultureID;
        self.m_CultureTable := CCultureTable.Default
      end
    end
    else
    begin
      table := nil;
      _actualName := nil;
      if (CCultureInfo.GetSortID(cultureID) > 0) then
        dataItemFromCultureID := CCultureTable.Default.GetDataItemFromCultureID(CCultureInfo.GetLangID(cultureID), _actualName);
      if (dataItemFromCultureID < 0) then
        _actualName := CString(CCultureTableRecord.SyntheticLcidToNameCache.Item[CCultureInfo.GetLangID(cultureID)]);
      str5 := CString(CCultureTableRecord.SyntheticLcidToNameCache.Item[cultureID]);
      dataItem := -1;

//      if (((str5 <> nil) and (_actualName <> nil)) and not Environment.GetCompatibilityFlag(CompatibilityFlag.DisableReplacementCustomCulture)) then
//        table := self.TryCreateReplacementCulture(actualName, @(dataItem));

      if (table = nil) then
      begin
        if (dataItemFromCultureID <= 0) then
        begin
          if (self.GetSyntheticCulture(cultureID)) then
            exit
        end
        else
        begin
          self.m_CultureTable := CCultureTable.Default;
          self.m_ActualCultureID := cultureID;
          self.m_synthetic := true;
          self.m_ActualName := CCultureInfo.nativeGetCultureName(cultureID, true, false)
        end
      end
      else
      begin
        self.m_CultureTable := table;
        dataItemFromCultureID := dataItem;
        self.m_ActualName := CCultureInfo.nativeGetCultureName(cultureID, true, false);
        self.m_ActualCultureID := cultureID
      end;
    end;
  end;

  if (dataItemFromCultureID >= 0) then
  begin
//      self.m_pData := ((self.m_CultureTable.m_pItemData + (self.m_CultureTable.m_itemSize * dataItemFromCultureID)) as CultureTableData*);
//      self.m_pPool := self.m_CultureTable.m_pDataPool;
    self.m_CultureName := self.SNAME;

    if (self.m_ActualCultureID = $40a) then
      self.m_CultureID :=  self.m_ActualCultureID else
      self.m_CultureID := self.ILANGUAGE;

//      self.CheckCustomSynthetic
  end
  else
  begin
    if (cultureName <> nil) then
      raise ArgumentException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Argument_InvalidCultureName'), [cultureName]), 'name');
    raise ArgumentException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Argument_CultureNotSupported'), [cultureID]), 'culture')
  end
end;

class function CCultureTableRecord.get_InternalSyncObject: TObject;
begin
  Result := syncObj;
end;

function CCultureTableRecord.get_ActualName: CString;
begin
  if (self.m_ActualName = nil) then
    self.m_ActualName := self.SNAME;
  begin
    Result := self.m_ActualName;
    exit
  end
end;

function CCultureTableRecord.get_CultureID: Integer;
begin
  Result := self.m_CultureID;
end;

function CCultureTableRecord.get_CultureName: CString;
begin
  Result := m_CultureName;
end;

procedure CCultureTableRecord.set_CultureName(const Value: CString);
begin
  raise NotImplementedException.Create;
end;

function CCultureTableRecord.get_IsNeutralCulture: Boolean;
begin
  Result := ((self.IFLAGS and 1) = 0);
end;

function CCultureTableRecord.get_ICALENDARTYPE: Integer;
begin
  {$IFDEF CROSSVCL}
  Result := LOCALE_USER_DEFAULT;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  GetLocaleInfoW(CultureID, LOCALE_ICALENDARTYPE or LOCALE_RETURN_NUMBER, Pointer(@Result), SizeOf(Result));
  {$ENDIF}
  {$ENDIF}
end;

function CCultureTableRecord.get_ICOMPAREINFO: Cardinal;
begin
  raise NotImplementedException.Create;
end;

function CCultureTableRecord.get_SDAYNAMES: StringArray;
var
  fs: TFormatSettings;

begin
  SetLength(Result, 7);

  fs := TFormatSettings.Create;
  Result[Integer(DayOfWeek.Sunday)] := fs.LongDayNames[1];
  Result[Integer(DayOfWeek.Monday)] := fs.LongDayNames[2];
  Result[Integer(DayOfWeek.Tuesday)] := fs.LongDayNames[3];
  Result[Integer(DayOfWeek.Wednesday)] := fs.LongDayNames[4];
  Result[Integer(DayOfWeek.Thursday)] := fs.LongDayNames[5];
  Result[Integer(DayOfWeek.Friday)] := fs.LongDayNames[6];
  Result[Integer(DayOfWeek.Saturday)] := fs.LongDayNames[7];
end;

function CCultureTableRecord.get_IFIRSTDAYOFWEEK: Integer;
{$IFNDEF MSWINDOWS}
var
  ii: IInterface;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  GetLocaleInfoW(CultureID, LOCALE_IFIRSTDAYOFWEEK or LOCALE_RETURN_NUMBER, Pointer(@Result), SizeOf(Result));
  {$ELSE}
  ii := TPlatformServices.Current.GetPlatformService(IFMXLocaleService);
  if ii <> nil then
    Result := (ii as IFMXLocaleService).GetFirstWeekday else
    Result := 1;
  {$ENDIF}
end;

function CCultureTableRecord.get_IFIRSTWEEKOFYEAR: Integer;
begin
  {$IFDEF MSWINDOWS}
  GetLocaleInfoW(CultureID, LOCALE_IFIRSTWEEKOFYEAR or LOCALE_RETURN_NUMBER, Pointer(@Result), SizeOf(Result));
  {$ENDIF}
end;

function CCultureTableRecord.get_IFLAGS: Integer;
begin
  Result := 1;
end;

function CCultureTableRecord.CloneWithUserOverride(userOverride: boolean): CultureTableRecord;
begin
  raise NotImplementedException.Create;
end;

class function CCultureTableRecord.GetCultureTableRecord(name: CString; useUserOverride: boolean): CultureTableRecord;
var
  index: Integer;
  num2: Integer;
  recordArray: CultureTableRecordArray;
  _record: CultureTableRecord;
//  cultureTableRecordObj: CObject;

begin
  if (CCultureTableRecord.CultureTableRecordCache = nil) then
  begin
    if (name.Length = 0) then
      begin
        Result := CCultureTableRecord.Create(name, useUserOverride);
        exit
      end;
    lock (CCultureTableRecord.InternalSyncObject);
    begin
      if (CCultureTableRecord.CultureTableRecordCache = nil) then
        CCultureTableRecord.CultureTableRecordCache := CDictionary<CString, CultureTableRecordArray>.Create;
      end
    end;
  name := CCultureTableRecord.ValidateCulturePieceToLower(name, 'name', $54);

  if CCultureTableRecord.CultureTableRecordCache.TryGetValue(name, recordArray) then
  begin
    if useUserOverride then
      index :=  0 else
      index :=  1;

    if (recordArray[index] = nil) then
    begin
      if (index = 0) then
        num2 := 1 else
        num2 := 0;

      _record := recordArray[num2];
      recordArray[index] := _record.CloneWithUserOverride(useUserOverride);
    end;
    begin
      Result := recordArray[index];
      exit
    end
  end;
  _record := CCultureTableRecord.Create(name, useUserOverride);
  lock (CCultureTableRecord.InternalSyncObject);
  begin
    if not CCultureTableRecord.CultureTableRecordCache.ContainsKey(name) then
    begin
      SetLength(recordArray, 2);

      if useUserOverride then
        recordArray[0] := _record else
        recordArray[1] := _record;
      CCultureTableRecord.CultureTableRecordCache.Item[name] := recordArray;
    end
  end;
  begin
    Result := _record;
    exit
  end
end;

class function CCultureTableRecord.GetCultureTableRecord(cultureId: Integer; useUserOverride: boolean): CultureTableRecord;
var
  actualName: CString;

begin
  if (cultureId = $7f) then
    begin
      Result := CCultureTableRecord.GetCultureTableRecord('', false);
      exit
    end;

//  actualName := nil;
//  if ((CultureTable.Default.GetDataItemFromCultureID(cultureId, actualName) < 0) and CultureInfo.IsValidLCID(cultureId, 1)) then
//  begin
//    CCultureTableRecord.InitSyntheticMapping;
//    actualName := CString(CCultureTableRecord.SyntheticLcidToNameCache.Item[cultureId])
//  end;
//  if ((CultureTable.Default.GetDataItemFromCultureID(cultureId, actualName) < 0) and CultureInfo.IsValidLCID(cultureId, 1)) then
// .Net

  actualName := CCultureTableRecord.StoreLCIDNamePair(cultureId);

  if ((actualName = nil) or (actualName.Length <= 0)) then
    raise ArgumentException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Argument_CultureNotSupported'), [cultureId]), 'culture');
  begin
    Result := CCultureTableRecord.GetCultureTableRecord(actualName, useUserOverride);
    exit
  end;
end;

//function CCultureTableRecord.GetDataItemFromCultureID(cultureID: Integer; var actualName: CString): Integer;
//begin
//  raise NotImplementedException.Create;
//end;

function CCultureTableRecord.GetString(LCType: Cardinal; maxLength: Integer) : CString;
var
  buf: CharArray;
  L: Integer;

begin
  Assert(CultureID > 0);
  {$IFDEF MSWINDOWS}
  SetLength(buf, maxLength);

  if not m_bUseUserOverride then
    LCType := LCType or LOCALE_NOUSEROVERRIDE;

  L := GetLocaleInfo(CultureID, LCType, Pointer(buf), maxLength);

  if L > 1 then
    Result := CString.Create(buf, 0, L - 1) else
    Result := nil;
    //raise CException.Create(Environment.GetResourceString('GetLocaleInfo_Failed'));
  {$ENDIF}
end;

function CCultureTableRecord.GetSyntheticCulture(cultureID: Integer): boolean;
begin
  Result := False;
end;

class procedure CCultureTableRecord.InitSyntheticMapping;
begin
  if SyntheticLcidToNameCache = nil then
    SyntheticLcidToNameCache := CHashTable.Create;
  if SyntheticNameToLcidCache = nil then
    SyntheticNameToLcidCache := CHashTable.Create;
end;

class function CCultureTableRecord.StoreLCIDNamePair(cultureID: Integer) : CString;
var
  name: CString;
begin
  InitSyntheticMapping;

  Result := CCultureInfo.nativeGetCultureName(cultureID, false, false);
  SyntheticLcidToNameCache[cultureID] := Result;

  name := ValidateCulturePieceToLower(Result, 'name', $54);
  SyntheticNameToLcidCache[name] := cultureID;
end;

class function CCultureTableRecord.IsCustomCultureId(cultureId: Integer): boolean;
begin
  if ((cultureId <> $c00) and (cultureId <> $1000)) then
    begin
      Result := false;
      exit
    end;
  begin
    Result := true;
    exit
  end
end;

class function CCultureTableRecord.ValidateCulturePieceToLower(const testString: CString; const paramName: CString; maxLength: Integer): CString;
var
  builder: StringBuilder;
  ch: CChar;
  i: Integer;
begin
  if (testString.Length > maxLength) then
    raise ArgumentException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Argument_NameTooLong'), [testString, maxLength]), paramName);
  builder := CStringBuilder.Create(testString.Length);
  i := 0;
  while ((i < testString.Length)) do
  begin
    ch := testString.Chars[i];
    if ((ch <= 'Z') and (ch >= 'A')) then
      builder.Append(CChar(SystemChar((Integer(SystemChar(ch)) - Integer(SystemChar('A'))) + $61)))
    else
    begin
      if ((((ch > 'z') or (ch < 'a')) and ((ch > '9') or (ch < '0'))) and ((ch <> '_') and (ch <> '-'))) then
        raise ArgumentException.Create(CString.Format(CCultureInfo.CurrentCulture, Environment.GetResourceString('Argument_NameContainsInvalidCharacters'), [testString]), paramName);
      builder.Append(ch)
    end;
    inc(i)
  end;
  begin
    Result := builder.ToString;
    exit
  end
end;

function CCultureTableRecord.get_SABBREVDAYNAMES: StringArray;
var
  fs: TFormatSettings;
begin
  SetLength(Result, 7);

  fs := TFormatSettings.Create;
  Result[Integer(DayOfWeek.Sunday)] := fs.ShortDayNames[1];
  Result[Integer(DayOfWeek.Monday)] := fs.ShortDayNames[2];
  Result[Integer(DayOfWeek.Tuesday)] := fs.ShortDayNames[3];
  Result[Integer(DayOfWeek.Wednesday)] := fs.ShortDayNames[4];
  Result[Integer(DayOfWeek.Thursday)] := fs.ShortDayNames[5];
  Result[Integer(DayOfWeek.Friday)] := fs.ShortDayNames[6];
  Result[Integer(DayOfWeek.Saturday)] := fs.ShortDayNames[7];
end;

function CCultureTableRecord.get_SABBREVMONTHNAMES: StringArray;
var
  fs: TFormatSettings;
  m: Integer;

begin
  SetLength(Result, 12);

  fs := TFormatSettings.Create;
  for m := 0 to 11 do
    Result[m] := fs.ShortMonthNames[m+1];
end;

function CCultureTableRecord.get_SABBREVMONTHGENITIVENAMES: StringArray;
{$IFDEF MSWINDOWS}
var
  buf: CharArray;
  m: Integer;
  ft: Int64;
  l: Integer;
  sys: SYSTEMTIME;
{$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  SetLength(buf, 82);
  SetLength(Result, 12);

  for m := 1 to 12 do
  begin
    {$IFDEF CROSSVCL}
    Result[m - 1] := m.ToString;
    {$ELSE}
    ft := CDateTime.Create(CDateTime.Create(2009, m, 1).Ticks, DateTimeKind.Utc).ToFileTime;
    FileTimeToSystemTime(_FILETIME((@ft)^), sys);

    l := GetDateFormat(  CultureID,
                    0,
                    @sys,
                    'ddMMM',
                    Pointer(Buf),
                    82);
    Result[m - 1] := CString.Create(buf, 0, l).Substring(2);
    {$ENDIF}
  end;
  {$ELSE}
  raise NotImplementedException.Create;
  {$ENDIF}
end;

function CCultureTableRecord.get_S1159: CString;
begin
  {$IFDEF MSWINDOWS}
  Result := Self.GetString(LOCALE_S1159, 15);
  {$ELSE}
  Result := FormatSettings.TimeAMString;
  {$ENDIF}
end;

function CCultureTableRecord.get_S2359: CString;
begin
  {$IFDEF MSWINDOWS}
  Result := Self.GetString(LOCALE_S2359, 12);
  {$ELSE}
  Result := FormatSettings.TimePMString;
  {$ENDIF}
end;

function CCultureTableRecord.get_SADERA: CString;
begin
  raise NotImplementedException.Create;
end;

function CCultureTableRecord.get_SDATE: CString;
begin
  {$IFDEF MSWINDOWS}
  Result := GetString(LOCALE_SDATE, 4);
  {$ELSE}
  Result := FormatSettings.DateSeparator;
  {$ENDIF}
end;

function CCultureTableRecord.get_ILANGUAGE: Word;
var
  s: CString;
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  s := GetString(LOCALE_ILANGUAGE, 5);
  if not CString.IsNullOrEmpty(s) then
    Result := CInteger.Parse(s);
  {$ELSE}
  raise NotImplementedException.Create;
  {$ENDIF}
end;

function CCultureTableRecord.get_SLONGDATE: CString;
begin
  {$IFDEF MSWINDOWS}
  Result := GetString(LOCALE_SLONGDATE, 80);
  {$ELSE}
  // System.SysUtils TFormatSettings.Create where Short-&LongDate formatting is going ToLower, which results in Months -> minutes
  Result := FormatSettings.LongDateFormat;
  Result := Result.Replace('m', 'M');
  {$ENDIF}
end;

function CCultureTableRecord.get_SNAME: CString;
const
  LOCALE_SNAME = $0000005c;

begin
  Result := GetString(LOCALE_SNAME, 85);
end;

function CCultureTableRecord.get_SMONTHDAY: CString;
begin
  Result := 'dd MMMM';
end;

function CCultureTableRecord.get_SMONTHGENITIVENAMES: StringArray;
var
  buf: CharArray;
  m: Integer;
{$IFDEF MSWINDOWS}
  ft: Int64;
  l: Integer;
  sys: SYSTEMTIME;
{$ENDIF}
begin
  SetLength(buf, 82);
  SetLength(Result, 12);
  {$IFDEF MSWINDOWS}
  for m := 1 to 12 do
  begin
    {$IFDEF CROSSVCL}
    Result[m - 1] := m.ToString;
    {$ELSE}
    ft := CDateTime.Create(CDateTime.Create(2009, m, 1).Ticks, DateTimeKind.Utc).ToFileTime;
    FileTimeToSystemTime(_FILETIME((@ft)^), sys);

    l := GetDateFormat(  CultureID,
                    0,
                    @sys,
                    'ddMMMM',
                    Pointer(Buf),
                    82);
    Result[m - 1] := CString.Create(buf, 0, l - 1 {strip #0}).Substring(2);
    {$ENDIF}
  end;
  {$ELSE}
  var fs := TFormatSettings.Create;
  for m := 1 to 12 do
  begin
    var cd := CDateTime.Create(2009, m, 1);
    FormatDateTime('mmmm', cd, fs);
    Result[m - 1] := fs.LongMonthNames[m];
  end;
  {$ENDIF}
end;

function CCultureTableRecord.get_SMONTHNAMES: StringArray;
var
  m: Integer;
  {$IFNDEF MSWINDOWS}
  fs: TFormatSettings;
  {$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  SetLength(Result, 13);
  for m := 0 to 12 do // 13 Months -> some calendars have 13 months per year!!
    Result[m] := GetString(LOCALE_SMONTHNAME1 + m, 80);
  {$ELSE}
  SetLength(Result, 12);
  fs := TFormatSettings.Create;
  for m := 0 to 11 do
    Result[m] := fs.LongMonthNames[m+1];
  {$ENDIF}
end;

function CCultureTableRecord.get_SSHORTDATE: CString;
begin
  {$IFDEF MSWINDOWS}
  Result := GetString(LOCALE_SSHORTDATE, 80);
  {$ELSE}
  // System.SysUtils TFormatSettings.Create where Short-&LongDate formatting is going ToLower, which results in Months -> minutes
  Result := FormatSettings.ShortDateFormat;
  Result := Result.Replace('m', 'M');
  {$ENDIF}
end;

function CCultureTableRecord.get_STIME: CString;
begin
  {$IFDEF MSWINDOWS}
  Result := GetString(LOCALE_STIME, 4);
  {$ELSE}
  Result := FormatSettings.TimeSeparator;
  {$ENDIF}
end;

function CCultureTableRecord.get_SSHORTTIME: CString;
begin
  Result := CString.Concat(CString('H'), STIME, CString('mm'));
//  Result := GetString(LOCALE_STIMEFORMAT, 80);
end;

function CCultureTableRecord.get_STIMEFORMAT: CString;
begin
  {$IFDEF MSWINDOWS}
  Result := GetString(LOCALE_STIMEFORMAT, 80);
  {$ELSE}
  Result := FormatSettings.LongTimeFormat;
  {$ENDIF}
end;

function CCultureTableRecord.get_SYEARMONTHS: StringArray;
begin
  raise NotImplementedException.Create;
end;

function CCultureTableRecord.get_SYEARMONTH: CString;
const
  LOCALE_SYEARMONTH = $00001006;   // year month format string
begin
  {$IFDEF MSWINDOWS}
  Result := GetString(LOCALE_SYEARMONTH, 80);
  {$ELSE}
  Result := 'MMMM yyyy';
  {$ENDIF}
end;

{ CultureInfo }

class procedure CCultureInfo._Create;
var
  cultureObj: CCultureInfo;
begin
  if (CCultureInfo.m_InvariantCultureInfo = nil) then
  begin
    cultureObj := CCultureInfo.Create($7f, false);
    CCultureInfo.m_InvariantCultureInfo := cultureObj;
    cultureObj.m_isReadOnly := true;
  end;
  CCultureInfo.m_userDefaultCulture := CCultureInfo.m_InvariantCultureInfo;
  CCultureInfo.m_userDefaultUICulture := CCultureInfo.m_InvariantCultureInfo;
  CCultureInfo.m_userDefaultCulture := CCultureInfo.InitUserDefaultCulture;
  CCultureInfo.m_userDefaultUICulture := CCultureInfo.InitUserDefaultUICulture
end;

constructor CCultureInfo.Create(culture: Integer; useUserOverride: boolean);
begin
  if (culture < 0) then
    raise ArgumentOutOfRangeException.Create('culture', Environment.GetResourceString('ArgumentOutOfRange_NeedPosNum'));
  case culture of
    $800,
    $c00,
    $1000,
    0,
    $400:
      begin
        raise ArgumentException.Create(Environment.GetResourceString('Argument_CultureNotSupported', [culture]), 'culture')
      end;
  end;
  self.cultureID := culture;
  self.m_cultureTableRecord := CCultureTableRecord.GetCultureTableRecord(self.cultureID, useUserOverride);
  self.m_name := self.m_cultureTableRecord.ActualName;
  self.m_isInherited := (inherited GetType <> Global.GetTypeOf<CultureInfo>)
end;

constructor CCultureInfo.Create(culture: Integer);
begin
  self.cultureID := culture;
  self.m_cultureTableRecord := CCultureTableRecord.GetCultureTableRecord(self.cultureID, true);
  self.m_name := self.m_cultureTableRecord.ActualName;
  self.m_isInherited := (inherited GetType <> Global.GetTypeOf<CultureInfo>)
end;

class procedure CCultureInfo.CheckNeutral(culture: CultureInfo);
begin
  if (culture.IsNeutralCulture) then
    raise NotSupportedException.Create(Environment.GetResourceString('Argument_CultureInvalidFormat', [(culture.GetObject as CCultureInfo).m_name]));
end;

class function CCultureInfo.CurrentCulture: CultureInfo;
begin
  Result := Thread.CurrentThread.CurrentCulture;
end;

class function CCultureInfo.CurrentUICulture: CultureInfo;
begin
  Result := Thread.CurrentThread.CurrentUICulture;
end;

class function CCultureInfo.InvariantCulture: CultureInfo;
begin
  Result := CCultureInfo.m_InvariantCultureInfo;
end;

class function CCultureInfo.UserDefaultCulture: CultureInfo;
var
  _userDefaultCulture: CultureInfo;

begin
  _userDefaultCulture := CCultureInfo.m_userDefaultCulture;
  if (_userDefaultCulture = nil) then
  begin
    CCultureInfo.m_userDefaultCulture := CCultureInfo.InvariantCulture;
    _userDefaultCulture := CCultureInfo.InitUserDefaultCulture;
    CCultureInfo.m_userDefaultCulture := _userDefaultCulture
  end;
  begin
    Result := _userDefaultCulture;
    exit
  end
end;

class function CCultureInfo.UserDefaultUICulture: CultureInfo;
var
  _userDefaultUICulture: CultureInfo;

begin
  _userDefaultUICulture := CCultureInfo.m_userDefaultUICulture;
  if (_userDefaultUICulture = nil) then
  begin
    CCultureInfo.m_userDefaultUICulture := CCultureInfo.InvariantCulture;
    _userDefaultUICulture := CCultureInfo.InitUserDefaultUICulture;
    CCultureInfo.m_userDefaultUICulture := _userDefaultUICulture
  end;
  begin
    Result := _userDefaultUICulture;
    exit
  end
end;

class function CCultureInfo.GetLangID(culture: Integer): Integer;
begin
  Result := (culture and $ffff)
end;

class function CCultureInfo.GetSortID(lcid: Integer): Integer;
begin
  Result := ((lcid shr $10) and 15);
end;

class function CCultureInfo.IsValidLCID(LCID: Integer; flag: Integer): boolean;
begin
  Result := True;
end;

function CCultureInfo.get_NumberFormat: NumberFormatInfo;
begin
  if numInfo = nil then
  begin
    numinfo := CNumberFormatInfo.Create;
    if cultureID = $7f {Invariant} then
      numInfo.NumberDecimalSeparator := '.';
  end;

  Result := numInfo;
end;

procedure CCultureInfo.set_NumberFormat(const Value: NumberFormatInfo);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_Obj'));
  self.numInfo := value
end;

function CCultureInfo.get_Calendar: Calendar;
var
  calendarInstance: System.Globalization.Interfaces.Calendar;
begin
  if (self._calendar = nil) then
  begin
    calendarInstance := CCultureInfo.GetCalendarInstance(self.m_cultureTableRecord.ICALENDARTYPE);
//    Thread.MemoryBarrier;
    calendarInstance.SetReadOnlyState(self.m_isReadOnly);
    self._calendar := calendarInstance
  end;
  begin
    Result := self._calendar;
    exit
  end
end;

function CCultureInfo.GetFormat(const formatType: &Type): IBaseInterface;
begin
 if (formatType = Global.GetTypeOf<NumberFormatInfo>) then
    begin
      Result := self.NumberFormat;
      exit
    end;
  if (formatType = Global.GetTypeOf<DateTimeFormatInfo>) then
    begin
      Result := self.DateTimeFormat;
      exit
    end;
  begin
    Result := nil;
    exit
  end
end;

function CCultureInfo.GetFormat(AClass: TClass): IBaseInterface;
begin
  Result := nil
end;

class function CCultureInfo.nativeGetCultureName(lcid: Integer; useSNameLCType: boolean; getMonthName: boolean): CString;
{$IFDEF MSWINDOWS}
var
  buf: CharArray;
  length: Integer;
  tmp: CString;
{$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  SetLength(buf, 9);
  length := GetLocaleInfoW(lcid,  LOCALE_SISO639LANGNAME, Pointer(buf), 9);
  tmp := CString.Create(buf, 0, length - 1);

  length := GetLocaleInfoW(lcid, LOCALE_SISO3166CTRYNAME, Pointer(buf), 9);
  Result := CString.Concat(tmp, '-', CString.Create(buf, 0, length - 1));
  {$ELSE}
  var ii: IFMXLocaleService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, ii) then
    Result := (ii as IFMXLocaleService).GetCurrentLangID else
    Result := 'EN';
  {$ENDIF}
end;

class function CCultureInfo.nativeGetSystemDefaultUILanguage(var LCID: Integer): CString;
  {$IFDEF MSWINDOWS}
  {$ELSE}
var
  ii: IInterface;
  {$ENDIF}

begin
  {$IFDEF CROSSVCL}
  LCID := 1043;
  Result := nil;
  {$ELSE}
    {$IFDEF MSWINDOWS}
    LCID := GetSystemDefaultUILanguage;
    Result := nil;
    {$ELSE}
    LCID := 1043;
    Result := nil;
    {$ENDIF}
  {$ENDIF}
end;

class function CCultureInfo.nativeGetUserDefaultLCID(var LCID: Integer; lcidType: Integer): CString;
begin
  {$IFDEF MSWINDOWS}
  {$IFNDEF CROSSVCL}
  if lcidType = LOCALE_USER_DEFAULT then
    LCID := GetUserDefaultLCID
  else if lcidType = LOCALE_SYSTEM_DEFAULT then
    LCID := GetSystemDefaultLCID
  else
    LCID := 0;
  {$ENDIF}
  {$ENDIF}

//  var buf: CharArray;
//  var length: Integer;
//  SetLength(buf, LOCALE_NAME_MAX_LENGTH);
//  length := GetUserDefaultLocaleName(Pointer(buf), LOCALE_NAME_MAX_LENGTH);
//  if length = 0 then
//    RaiseLastOsError;
//  Result := CString.Create(buf, 0, length);
end;

class function CCultureInfo.nativeGetUserDefaultUILanguage(var LCID: Integer): CString;
begin
  {$IFDEF MSWINDOWS}
  {$IFNDEF CROSSVCL}
  LCID := GetUserDefaultUILanguage;
  Result := nil;
  {$ENDIF}
  {$ENDIF}
end;

class function CCultureInfo.GetCalendarInstance(calType: Integer): Calendar;
begin
  if (calType = 1) then
    begin
      Result := CGregorianCalendar.Create;
      exit
    end;
  begin
    Result := CCultureInfo.GetCalendarInstanceRare(calType);
    exit
  end
end;

class function CCultureInfo.GetCalendarInstanceRare(calType: Integer): Calendar;
begin
  case calType of
    2,9,10,11,12:
    begin
      begin
        Result := CGregorianCalendar.Create(GregorianCalendarTypes(calType));
        exit
      end
    end;
  end;

  // KV: 5-1-2015
  // Nomatter what, allways return a calendar....
  Exit(CGregorianCalendar.Create(GregorianCalendarTypes(calType)));
  raise CException.Create(CString.Format('Calendars of type {0} are currently not supported. Please contact A-Dato for an update of your ssystem.', calType));
end;

class function CCultureInfo.GetCultureByLCIDOrName(preferLCID: Integer; const fallbackToString: CString): CultureInfo;
var
  info: CultureInfo;

begin
  info := CCultureInfo.Create(preferLCID);
  Result := info;
  {$IFDEF KV_9_5_2021}
  info := nil;

  if ((preferLCID and $3ff) <> 0) then
    try
      info := CCultureInfo.Create(preferLCID)
    except
      on exception1: ArgumentException do

    end;
  if (((info = nil) and (fallbackToString <> nil)) and (fallbackToString.Length > 0)) then
    try
      info := CCultureInfo.Create(fallbackToString)
    except
      on exception2: ArgumentException do

    end;
  begin
    Result := info;
    exit
  end
  {$ENDIF}
end;

class function CCultureInfo.InitUserDefaultCulture: CultureInfo;
var
  num: Integer;
  fallbackToString: CString;
  cultureByLCIDOrName: CultureInfo;

begin
  {$IFDEF MSWINDOWS}
  fallbackToString := CCultureInfo.nativeGetUserDefaultLCID(num, $400);
  cultureByLCIDOrName := CCultureInfo.GetCultureByLCIDOrName(num, fallbackToString);
  if (cultureByLCIDOrName = nil) then
  begin
    fallbackToString := CCultureInfo.nativeGetUserDefaultLCID(num, $800);
    cultureByLCIDOrName := CCultureInfo.GetCultureByLCIDOrName(num, fallbackToString);
    if (cultureByLCIDOrName = nil) then
      begin
        Result := CCultureInfo.InvariantCulture;
        exit
      end
    end;
  (cultureByLCIDOrName.GetObject as CCultureInfo).m_isReadOnly := true;
  begin
    Result := cultureByLCIDOrName;
    exit
  end
  {$ELSE}
  Result := CCultureInfo.Create(43 {Just some value});
  {$ENDIF}
end;

class function CCultureInfo.InitUserDefaultUICulture: CultureInfo;
var
  cultureByLCIDOrName: CultureInfo;
  fallbackToString: CString;
  num: Integer;
begin
  fallbackToString := CCultureInfo.nativeGetUserDefaultUILanguage(num);
  if ((num = CCultureInfo.UserDefaultCulture.LCID) or (fallbackToString = CCultureInfo.UserDefaultCulture.Name)) then
    begin
      Result := CCultureInfo.UserDefaultCulture;
      exit
    end;
  cultureByLCIDOrName := CCultureInfo.GetCultureByLCIDOrName(num, fallbackToString);
  if (cultureByLCIDOrName = nil) then
  begin
    fallbackToString := CCultureInfo.nativeGetSystemDefaultUILanguage(num);
    cultureByLCIDOrName := CCultureInfo.GetCultureByLCIDOrName(num, fallbackToString)
  end;
  if (cultureByLCIDOrName = nil) then
    begin
      Result := CCultureInfo.InvariantCulture;
      exit
    end;
  (cultureByLCIDOrName.GetObject as CCultureInfo).m_isReadOnly := true;
  begin
    Result := cultureByLCIDOrName;
    exit
  end
end;

function CCultureInfo.get_DateTimeFormat: DateTimeFormatInfo;
var
  info: DateTimeFormatInfo;
begin
  if (self.dateTimeInfo = nil) then
  begin
    CCultureInfo.CheckNeutral(self);
    info := CDateTimeFormatInfo.Create(self.m_cultureTableRecord, CCultureInfo.GetLangID(self.cultureID), self.Calendar);
    (info.GetObject as CDateTimeFormatInfo).m_isReadOnly := self.m_isReadOnly;
//    Thread.MemoryBarrier;
    self.dateTimeInfo := info
  end;
  begin
    Result := self.dateTimeInfo;
    exit
  end
end;

procedure CCultureInfo.set_DateTimeFormat(const Value: DateTimeFormatInfo);
begin
  self.VerifyWritable;
  if (value = nil) then
    raise ArgumentNullException.Create('value', Environment.GetResourceString('ArgumentNull_Obj'));
  self.dateTimeInfo := value
end;

function CCultureInfo.get_IsNeutralCulture: boolean;
begin
  Result := self.m_cultureTableRecord.IsNeutralCulture
end;

function CCultureInfo.get_LCID: Integer;
begin
  Result := self.cultureID
end;

function CCultureInfo.get_Name: CString;
begin
 if (self.m_nonSortName = nil) then
    self.m_nonSortName := self.m_cultureTableRecord.CultureName;
  begin
    Result := self.m_nonSortName;
    exit
  end
end;

function CCultureInfo.get_Parent: CultureInfo;
begin
  Result := nil;
end;

class function CCultureInfo.VerifyCultureName(culture: CultureInfo; throwException: boolean): boolean;
var
  name: CString;
  c: CChar;
  i: Integer;

begin
  if ((culture.GetObject as CCultureInfo).m_isInherited) then
  begin
    name := culture.Name;
    i := 0;
    while ((i < name.Length)) do
    begin
      c := name.Chars[i];
      if ((not CChar.IsLetterOrDigit(c) and (c <> '-')) and (c <> '_')) then
      begin
        if (throwException) then
          raise ArgumentException.Create(Environment.GetResourceString('Argument_InvalidResourceCultureName', [name]));
        begin
          Result := false;
          exit
        end
      end;
      inc(i)
    end
  end;
  begin
    Result := true;
    exit
  end
end;

procedure CCultureInfo.VerifyWritable;
begin
  if (self.m_isReadOnly) then
    raise InvalidOperationException.Create(Environment.GetResourceString('InvalidOperation_ReadOnly'))
end;

class function HebrewNumber.ToString(Number: Integer): CString;
begin

end;

{ NumberFormatInfo }

constructor CNumberFormatInfo.Create;
begin
  _numberDecimalSeparator := FormatSettings.DecimalSeparator;
end;

destructor CNumberFormatInfo.Destroy;
begin
  inherited;
end;

class function CNumberFormatInfo.get_CurrentInfo: NumberFormatInfo;
begin
  if _CurrentInfo = nil then
  begin
    Lock(nil);
    if _CurrentInfo = nil then
      _CurrentInfo := CNumberFormatInfo.Create;
  end;

  Result := _CurrentInfo;
end;

class function CNumberFormatInfo.GetInstance(const formatProvider: IFormatProvider): NumberFormatInfo;
var
  info2: CCultureInfo;
  numInfo: NumberFormatInfo;
begin
  if (formatProvider <> nil) and (formatProvider.GetObject is CCultureInfo) then
    info2 := formatProvider.GetObject as CCultureInfo else
    info2 := nil;

  if ((info2 <> nil) and not info2.m_isInherited) then
  begin
    numInfo := info2.numInfo;
    if (numInfo <> nil) then
      begin
        Result := numInfo;
        exit
      end;
    begin
      Result := info2.NumberFormat;
      exit
    end
  end;

  if Interfaces.Supports(formatProvider, NumberFormatInfo, numInfo) then
  begin
    Result := numInfo;
    exit
  end;

  if (formatProvider <> nil) then
  begin
    if Interfaces.Supports(formatProvider.GetFormat(Global.GetTypeOf<NumberFormatInfo>), NumberFormatInfo, numInfo) then
    begin
      Result := numInfo;
      exit
    end
  end;

  begin
    Result := CNumberFormatInfo.CurrentInfo;
    exit
  end
end;

function CNumberFormatInfo.get_CurrencySymbol: CString;
begin
  Result := _CurrencySymbol;
end;

procedure CNumberFormatInfo.set_CurrencySymbol(const Value: CString);
begin
  _CurrencySymbol := Value;
end;

function CNumberFormatInfo.get_NaNSymbol: CString;
begin
  Result := _NaNSymbol;
end;

procedure CNumberFormatInfo.set_NaNSymbol(const Value: CString);
begin
  _NaNSymbol := Value;
end;

function CNumberFormatInfo.get_NegativeInfinitySymbol: CString;
begin
  Result := _NegativeInfinitySymbol;
end;

procedure CNumberFormatInfo.set_NegativeInfinitySymbol(const Value: CString);
begin
  _NegativeInfinitySymbol := Value;
end;

function CNumberFormatInfo.get_NumberDecimalSeparator: CString;
begin
  Result := self._numberDecimalSeparator;
end;

procedure CNumberFormatInfo.set_NumberDecimalSeparator(const Value: CString);
begin
//  self.VerifyWritable;
//  self.VerifyDecimalSeparator(value, 'NumberDecimalSeparator');
  self._numberDecimalSeparator := Value;
end;

function CNumberFormatInfo.get_PositiveInfinitySymbol: CString;
begin
  Result := _PositiveInfinitySymbol;
end;

procedure CNumberFormatInfo.set_PositiveInfinitySymbol(const Value: CString);
begin
  _PositiveInfinitySymbol := Value;
end;

procedure GlobalInitialization;
begin
  CCultureTableRecord.syncObj := TObject.Create;
  CalendarTable._Create;
  CCultureTable._Create;
  CCompareInfo._classLock := CCompareInfo.Create;
  CCultureInfo._Create;
  DateTimeFormat._Create;
end;

procedure GlobalFinalization;
begin
  CCultureTableRecord.syncObj.Free;
end;

end.

