unit BitConverter;

interface

uses System.Types;

type
  /// <summary> TBitConverter class implements converter of an array of bytes
  ///  into Delphi native data types and backward. </summary>
  TBitConverter = class
  protected
    /// <summary> RangeCheckError is a helper used to raise a range check exception
    ///  if a conversion (From or Into) accesses memory outside the bounds of the
    ///  Byte array.
    ///  NOTE: Only used by C++; Delphi calls System.Error(reRangeError) directly.</summary>
    class procedure RangeCheckError; static;
  public
    /// <summary> UnsafeFrom method converts Value of T type into array of bytes B.
    ///  The value will be written into B at Offset offset. The method does not check
    ///  that value will fit into B at Offset. </summary>
    class procedure UnsafeFrom<T>(const Value: T; var B: TArray<Byte>; Offset: Integer = 0); static; inline;
    /// <summary> From method converts Value of T type into array of bytes B.
    ///  The value will be written into B at Offset offset. The method checks that
    ///  value will fit into B at Offset. If not, then range check exception is raised. </summary>
    class procedure From<T>(const Value: T; var B: TArray<Byte>; Offset: Integer = 0); static;
    /// <summary> UnsafeInTo method converts array of bytes B into value of T type.
    ///  The value will be read from B at Offset offset. The method does not check
    ///  that value fits into B at Offset. </summary>
    class function UnsafeInTo<T>(const B: TArray<Byte>; Offset: Integer = 0): T; static; inline;
    /// <summary> InTo method converts array of bytes B into value of T type.
    ///  The value will be read from B at Offset offset. The method checks that
    ///  value fits into B at Offset. If not, then range check exception is raised. </summary>
    class function InTo<T>(const B: TArray<Byte>; Offset: Integer = 0): T; static;
  end;

  TDBBitConverter = class(TBitConverter)
  private
    class procedure InternalFromMove<T>(const Value: T; var B: TArray<Byte>; Offset: Integer = 0); static; inline;
    class function InternalIntoMove<T>(const B: TArray<Byte>; Offset: Integer = 0): T; static; inline;
  public
    /// <summary> UnsafeFromVariant method converts Value of variant type into array of bytes B. </summary>
    class procedure UnsafeFromVariant(const Value: Variant; var B: TArray<Byte>; Offset: Integer = 0); static; inline;
    /// <summary> UnsafeFromInterface method converts Value of interface type into array of bytes B. </summary>
    class procedure UnsafeFromInterface(const Value: IInterface; var B: TArray<Byte>; Offset: Integer = 0); static; inline;
    /// <summary> UnsafeInToVariant method converts array of bytes B into variant value. </summary>
    class function UnsafeInToVariant(const B: TArray<Byte>; Offset: Integer = 0): Variant; static; inline;
    /// <summary> UnsafeInToInterface method converts array of bytes B into interface value. </summary>
    class function UnsafeInToInterface(const B: TArray<Byte>; Offset: Integer = 0): IUnknown; static; inline;
  end;

implementation

{ TBitConverter }
class procedure TBitConverter.RangeCheckError;
begin
  System.Error(reRangeError);
end;

class procedure TBitConverter.UnsafeFrom<T>(const Value: T;
  var B: TArray<Byte>; Offset: Integer);
type
  PT = ^T;
begin
  PT(@B[Offset])^ := Value;
end;

class procedure TBitConverter.From<T>(const Value: T; var B: TArray<Byte>;
  Offset: Integer);
begin
  if (Offset < 0) or (Offset + SizeOf(T) > Length(B)) then
    System.Error(reRangeError);
  UnsafeFrom<T>(Value, B, Offset);
end;

class function TBitConverter.UnsafeInTo<T>(const B: TArray<Byte>;
  Offset: Integer): T;
type
  PT = ^T;
begin
  Result := PT(@B[Offset])^;
end;

class function TBitConverter.InTo<T>(const B: TArray<Byte>;
  Offset: Integer): T;
begin
  if (Offset < 0) or (Offset + SizeOf(T) > Length(B)) then
    System.Error(reRangeError);
  Result := UnsafeInTo<T>(B, Offset);
end;

{ TDBBitConverter }

class procedure TDBBitConverter.InternalFromMove<T>(const Value: T;
  var B: TArray<Byte>; Offset: Integer);
begin
  Move(Value, B[Offset], SizeOf(T));
end;

class procedure TDBBitConverter.UnsafeFromVariant(const Value: Variant;
  var B: TArray<Byte>; Offset: Integer);
begin
  InternalFromMove<Variant>(Value, B, Offset);
end;

class procedure TDBBitConverter.UnsafeFromInterface(const Value: IInterface;
  var B: TArray<Byte>; Offset: Integer);
begin
  InternalFromMove<IUnknown>(Value, B, Offset);
end;

class function TDBBitConverter.InternalIntoMove<T>(const B: TArray<Byte>;
  Offset: Integer): T;
begin
  Move(B[Offset], Result, SizeOf(T));
end;

class function TDBBitConverter.UnsafeInToVariant(const B: TArray<Byte>;
  Offset: Integer): Variant;
begin
  InternalIntoMove<Variant>(B, Offset);
end;

class function TDBBitConverter.UnsafeInToInterface(const B: TArray<Byte>;
  Offset: Integer): IUnknown;
begin
  InternalIntoMove<IUnknown>(B, Offset);
end;


end.
