namespace WinFormsHost
{
  partial class frmHost
  {
    /// <summary>
    /// Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose(bool disposing)
    {
      if (disposing && (components != null))
      {
        components.Dispose();
      }
      base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent()
    {
      this.nudLeft = new System.Windows.Forms.NumericUpDown();
      this.label1 = new System.Windows.Forms.Label();
      this.nudRight = new System.Windows.Forms.NumericUpDown();
      this.txtResult = new System.Windows.Forms.TextBox();
      this.label2 = new System.Windows.Forms.Label();
      this.btnCallDescriptionMethod = new System.Windows.Forms.Button();
      ((System.ComponentModel.ISupportInitialize)(this.nudLeft)).BeginInit();
      ((System.ComponentModel.ISupportInitialize)(this.nudRight)).BeginInit();
      this.SuspendLayout();
      // 
      // nudLeft
      // 
      this.nudLeft.Location = new System.Drawing.Point(12, 12);
      this.nudLeft.Name = "nudLeft";
      this.nudLeft.Size = new System.Drawing.Size(55, 20);
      this.nudLeft.TabIndex = 0;
      this.nudLeft.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
      this.nudLeft.ValueChanged += new System.EventHandler(this.ValueChanged);
      // 
      // label1
      // 
      this.label1.AutoSize = true;
      this.label1.Location = new System.Drawing.Point(73, 14);
      this.label1.Name = "label1";
      this.label1.Size = new System.Drawing.Size(13, 13);
      this.label1.TabIndex = 1;
      this.label1.Text = "+";
      // 
      // nudRight
      // 
      this.nudRight.Location = new System.Drawing.Point(92, 12);
      this.nudRight.Name = "nudRight";
      this.nudRight.Size = new System.Drawing.Size(55, 20);
      this.nudRight.TabIndex = 2;
      this.nudRight.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
      this.nudRight.ValueChanged += new System.EventHandler(this.ValueChanged);
      // 
      // txtResult
      // 
      this.txtResult.BackColor = System.Drawing.SystemColors.ButtonFace;
      this.txtResult.Location = new System.Drawing.Point(172, 12);
      this.txtResult.Name = "txtResult";
      this.txtResult.ReadOnly = true;
      this.txtResult.Size = new System.Drawing.Size(68, 20);
      this.txtResult.TabIndex = 3;
      this.txtResult.Text = "0";
      this.txtResult.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
      // 
      // label2
      // 
      this.label2.AutoSize = true;
      this.label2.Location = new System.Drawing.Point(153, 15);
      this.label2.Name = "label2";
      this.label2.Size = new System.Drawing.Size(13, 13);
      this.label2.TabIndex = 4;
      this.label2.Text = "=";
      // 
      // btnCallDescriptionMethod
      // 
      this.btnCallDescriptionMethod.Location = new System.Drawing.Point(58, 46);
      this.btnCallDescriptionMethod.Name = "btnCallDescriptionMethod";
      this.btnCallDescriptionMethod.Size = new System.Drawing.Size(149, 23);
      this.btnCallDescriptionMethod.TabIndex = 5;
      this.btnCallDescriptionMethod.Text = "Call ICalculator.Description";
      this.btnCallDescriptionMethod.UseVisualStyleBackColor = true;
      this.btnCallDescriptionMethod.Click += new System.EventHandler(this.btnCallDescriptionMethod_Click);
      // 
      // frmHost
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(252, 81);
      this.Controls.Add(this.btnCallDescriptionMethod);
      this.Controls.Add(this.label2);
      this.Controls.Add(this.txtResult);
      this.Controls.Add(this.nudRight);
      this.Controls.Add(this.label1);
      this.Controls.Add(this.nudLeft);
      this.MaximizeBox = false;
      this.Name = "frmHost";
      this.Text = "C#/WinForms Host";
      ((System.ComponentModel.ISupportInitialize)(this.nudLeft)).EndInit();
      ((System.ComponentModel.ISupportInitialize)(this.nudRight)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.NumericUpDown nudLeft;
    private System.Windows.Forms.Label label1;
    private System.Windows.Forms.NumericUpDown nudRight;
    private System.Windows.Forms.TextBox txtResult;
    private System.Windows.Forms.Label label2;
    private System.Windows.Forms.Button btnCallDescriptionMethod;
  }
}

