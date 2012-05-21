program CFMsgBoxDemo;

{$R *.res}

uses
  Macapi.CoreFoundation;

procedure ShowMessageCF(const AHeading, AMessage: string; const ATimeoutInSecs: Double = 0);
var
  LHeading, LMessage: CFStringRef;
  LResponse: CFOptionFlags;
begin
  LHeading := CFStringCreateWithCharactersNoCopy(nil, PWideChar(AHeading),
    Length(AHeading), kCFAllocatorNull);
  LMessage := CFStringCreateWithCharactersNoCopy(nil, PWideChar(AMessage),
    Length(AMessage), kCFAllocatorNull);
  try
    CFUserNotificationDisplayAlert(ATimeoutInSecs, kCFUserNotificationNoteAlertLevel,
      nil, nil, nil, LHeading, LMessage, nil, nil, nil, LResponse);
  finally
    CFRelease(LHeading);
    CFRelease(LMessage);
  end;
end;

begin
  ShowMessageCF('Test', 'This dialog will auto-destruct in 10 seconds!', 10);
end.
