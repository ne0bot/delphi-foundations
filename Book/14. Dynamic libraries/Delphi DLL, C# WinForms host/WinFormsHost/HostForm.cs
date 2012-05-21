using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace WinFormsHost
{
  public partial class frmHost : Form
  {
    ICalculator mCalc;

    public frmHost()
    {
      InitializeComponent();
      mCalc = DelphiLib.CreateCalculator();
    }

    private void ValueChanged(object sender, EventArgs e)
    {
      txtResult.Text = mCalc.AddThem((int)nudLeft.Value, (int) nudRight.Value).ToString();
    }

    private void btnCallDescriptionMethod_Click(object sender, EventArgs e)
    {
      MessageBox.Show(mCalc.Description());
    }
  }
}
