using System;
using System.Runtime.InteropServices;

namespace WinFormsHost
{
  [Guid("B1909D19-8DF8-4A4F-AF71-D0EFC653912F")]
  [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
  interface ICalculator
  {
    Int32 AddThem(Int32 Num1, Int32 Num2);
    string Description();
  }

  class DelphiLib
  {
    [DllImport("DelphiInterfaceTest.dll", PreserveSig = false)]
    public static extern ICalculator CreateCalculator();
  }
}
