unit GLShader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, GTURI;

type

  { TglShader }

  TGLShader = class (TObject)
  public
    constructor CreateFromURL(const AVertexShaderURL, AFragmentShaderURL: String);
    constructor CreateFromFile(const AVertexShaderFileName, AFragmentShaderFileName: String);
    constructor CreateFromStream(const AVertexShaderStream, AFragmentShaderStream: TStream);
    constructor CreateFromSource(const AVertexShaderSource, AFragmentShaderSource: String);
  private
    FProgramObject: GLHandle;
  protected
    class function CompileShader(const Kind: TGLenum; const Source: String): GLHandle;
  public
    procedure Bind;
    function GetUniformLocation(const UniformName: String): Integer;
    procedure Unbind;
  end;

implementation

{ TglShader }

constructor TGLShader.CreateFromURL(const AVertexShaderURL,
  AFragmentShaderURL: String);
var
  VFS, FFS: TStream;
begin
  VFS := TGTURIStream.ActualStream(AVertexShaderURL, omRead);
  try
    FFS := TGTURIStream.ActualStream(AFragmentShaderURL, omRead);
    try
      CreateFromStream(VFS, FFS);
    finally
      FFS.Free;
    end;
  finally
    VFS.Free;
  end;
end;

constructor TglShader.CreateFromFile(const AVertexShaderFileName,
  AFragmentShaderFileName: String);
var
  VFS, FFS: TFileStream;
begin
  VFS := TFileStream.Create(AVertexShaderFileName, fmOpenRead);
  try
    FFS := TFileStream.Create(AFragmentShaderFileName, fmOpenRead);
    try
      CreateFromStream(VFS, FFS);
    finally
      FFS.Free;
    end;
  finally
    VFS.Free;
  end;
end;

constructor TglShader.CreateFromStream(const AVertexShaderStream,
  AFragmentShaderStream: TStream);
var
  VSS, FSS: String;
begin
  SetLength(VSS, AVertexShaderStream.Size);
  SetLength(FSS, AFragmentShaderStream.Size);
  AVertexShaderStream.Read(VSS[1], Length(VSS));
  AFragmentShaderStream.Read(FSS[1], Length(FSS));
  CreateFromSource(VSS, FSS);
end;

constructor TglShader.CreateFromSource(const AVertexShaderSource,
  AFragmentShaderSource: String);
var
  VSObject, FSObject: GLHandle;
  Error: String;
begin
  VSObject := -1;
  FSObject := -1;
  FProgramObject := glCreateProgram();
  try
    VSObject := CompileShader(GL_VERTEX_SHADER, AVertexShaderSource);
    try
      FSObject := CompileShader(GL_FRAGMENT_SHADER, AFragmentShaderSource);
    except
      glDeleteShader(VSObject);
      raise;
    end;

    glAttachShader(FProgramObject, VSObject);
    glAttachShader(FProgramObject, FSObject);

    glLinkProgram(FProgramObject);
    if glGetError() <> GL_NO_ERROR then
    begin
      Error := StrPas(gluErrorString(glGetError()));
      glDeleteShader(VSObject);
      glDeleteShader(FSObject);
      raise Exception.CreateFmt('Linker error: %s', [Error]);
    end;
    glDeleteShader(VSObject);
    glDeleteShader(FSObject);
  except
    glDeleteProgram(FProgramObject);
    raise;
  end;
end;

class function TglShader.CompileShader(const Kind: TGLenum; const Source: String
  ): GLHandle;
var
  Src: PChar;
  Len: GLint;
  Error: String;
begin
  Src := @Source[1];
  Len := Length(Source);
  Result := glCreateShader(Kind);
  glShaderSource(Result, 1, @Src, @Len);
  glCompileShader(Result);

  glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @Len);
  if Len > 1 then
  begin
    SetLength(Error, Len);
    glGetShaderInfoLog(Result, Len, Len, @Error[1]);
    SetLength(Error, Len);
    glDeleteShader(Result);
    raise Exception.CreateFmt('Shader compilation failed: %s', [Error]);
  end;
end;

procedure TglShader.Bind;
begin
  glUseProgram(FProgramObject);
end;

function TglShader.GetUniformLocation(const UniformName: String): Integer;
begin
  Result := glGetUniformLocation(FProgramObject, @UniformName[1]);
  if Result < 0 then
    raise Exception.CreateFmt('Could not determine uniform location ''%s''.', [UniformName]);
end;

procedure TglShader.Unbind;
begin
  glUseProgram(0);
end;

end.

