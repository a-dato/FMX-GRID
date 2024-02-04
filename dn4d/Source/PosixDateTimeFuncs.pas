unit PosixDateTimeFuncs;

interface

uses
  Posix.Base;

type
  time_t = Longint;
  suseconds_t = Longint;

  timeval = record
    tv_sec: time_t;           { Seconds.  }
    tv_usec: suseconds_t;     { Microseconds.  }
  end;

  tm = record
    tm_sec: Integer;            // Seconds. [0-60] (1 leap second)
    tm_min: Integer;            // Minutes. [0-59]
    tm_hour: Integer;           // Hours.[0-23]
    tm_mday: Integer;           // Day.[1-31]
    tm_mon: Integer;            // Month.[0-11]
    tm_year: Integer;           // Year since 1900
    tm_wday: Integer;           // Day of week [0-6] (Sunday = 0)
    tm_yday: Integer;           // Days of year [0-365]
    tm_isdst: Integer;          // Daylight Savings flag [-1/0/1]
    tm_gmtoff: LongInt;         // Seconds east of UTC
    tm_zone: MarshaledAString;         // Timezone abbreviation
  end;

  {$EXTERNALSYM tm}
  Ptm = ^tm;

  function gettimeofday(var timeval: timeval; timezone: Pointer): Integer; cdecl; external libc name _PU + 'gettimeofday';
  function localtime_r(var Timer: time_t; var UnixTime: tm): Ptm; cdecl; overload; external libc name _PU + 'localtime_r';

implementation

end.
