
       Ctl-Opt DATEDIT(*YMD) FORMSALIGN(*NO) INDENT('| ') DECEDIT(*JOBRUN);
       Ctl-Opt DFTACTGRP(*NO) OPTION(*NODEBUGIO:*SRCSTMT) BNDDIR('QC2LE');
       Ctl-Opt ACTGRP(*CALLER);

      *---------------------------------------------------------------*
      *                                                               *
      * Application      : Loans                                      *
      * Module           : PayOff                                     *
      * Title            : Payoff Calculation                         *
      * Description      : This program will calculate the loan Payoff*
      *                    amount. works as a API                     *
      * Author           : Chamara Withanachchi                       *
      * Version          : V 1.0.0                                    *
      * Date             : 11/11/2016                                 *
      *---------------------------------------------------------------*
      *                                                               *
      *---------------------------------------------------------------*
      * MODIFICATIONS                                                 *
      *---------------------------------------------------------------*

       // Loan Master File
       Dcl-F LNP003 Disk Usage(*Input) Keyed;

       // Loan Control File
       Dcl-F LNP001 Disk Usage(*Input) Keyed;

       // MBSL Parameter File
       Dcl-F MPARM00 Disk Usage(*Input) Keyed;

       // Loan Schedule File
       Dcl-F LNP005L1 Disk Usage(*Input) Keyed;

       // Work Variable Definition
       // ----------------------------------------------------------------------
       Dcl-S payOffPersentage Packed(2:0);
       Dcl-S tmpDateISO Date(*ISO);
       Dcl-S pawnCalcDays Packed(3:0);
       Dcl-S cdbdPersentage Packed(3:0);
       Dcl-S leLEPersentage Packed(3:0);
       Dcl-S leGTPersentage Packed(3:0);
       Dcl-S wISOSysCutoDate Date(*ISO);
       Dcl-S wSystemCutoDate Like(LNNTDT);
       Dcl-S currentTerm Like(LNSCPD);
       Dcl-S chkCurrentTerm Like(LNSCPD);
       Dcl-S principleAccBalance Like(LNSPRN);
       Dcl-S currentSchedule Like(LNSCNR);

       // Program Status Data Structure .....
       // ----------------------------------------------------------------------
       Dcl-DS PgmInfo PSDS;
         #UserId Char(10) Pos(254);
         #JobName Char(10) Pos(244);
         #MsgID Char(7) Pos(40);
         #Parms *Parms;
         #PgmName *Proc;
       End-Ds;

       // Key Field Defiition
       // ----------------------------------------------------------------------
       Dcl-S kPKey1 Like(MBSKEY1);
       Dcl-S kPKey2 Like(MBSKEY2);
       Dcl-S kPKey3 Like(MBSKEY3);
       Dcl-S kPKey4 Like(MBSKEY4);
       Dcl-S kBank Like(LNBK);

       // Local Procedure definitions
       // ----------------------------------------------------------------------
       Dcl-PR iniProgram End-PR;

       Dcl-PR getParameters End-PR;

       Dcl-PR calcPayoff Like(LNBAL) End-PR;

       Dcl-PR ckeckProductOmit Char(1) End-PR;

       Dcl-PR ckeckPawningProd Char(1) End-PR;

       Dcl-PR getFutureInterest Packed(17:2) End-PR;

       // *Entry of the Program
       // ----------------------------------------------------------------------
       Dcl-PR loanPayoffCalc ExtPgm('MLN0050R');
         Dcl-Parm bankCode Like(LNBK);
         Dcl-Parm loanAccount Like(LNNOTE);
         Dcl-Parm loanPayOff Like(LNBAL);
       End-PR;

       Dcl-PI loanPayoffCalc;
         Dcl-Parm bankCode Like(LNBK);
         Dcl-Parm loanAccount Like(LNNOTE);
         Dcl-Parm loanPayOff Like(LNBAL);
       End-PI;

       // Main Progrm
       // ----------------------------------------------------------------------

       iniProgram();
       getParameters();
       loanPayOff = calcPayoff();

       *INLr = *On;

       // ----------------------------------------------------------------------
       // iniProgram : Initilize the program
       // ----------------------------------------------------------------------
       Dcl-Proc iniProgram;

         Dcl-PI iniProgram End-PI;

         kPKey1 = #PgmName;
         kBank = 1;

       End-Proc;

       // ----------------------------------------------------------------------
       // getParameters : Initilize the program
       // ----------------------------------------------------------------------
       Dcl-Proc getParameters;

         Dcl-PI getParameters End-PI;

         // Documentation Charges for Products
         kPKey2 = 'LOANPAYOFF';
         kPKey3 = 'CALCULATE';
         kPKey4 = 'PERSENT';

         Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
         If %Found(MPARM00);
           payOffPersentage = %Int(MBSVAL1);
         EndIf;

          // Get the System Live Date
          kPKey2 = 'SYSTEM';
          kPKey3 = 'LIVE';
          kPKey4 = 'CUTO_DATE';
          Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
          If %Found(MPARM00);
            wSystemCutoDate = %Int(MBSVAL1);
            If wSystemCutoDate <> *Zeros and wSystemCutoDate <> 9999999;
              wISOSysCutoDate =  %Date(wSystemCutoDate : *DMY);
            EndIf;
          EndIf;

         // Get the Current Date from Control File
         Chain kBank LNP0011;
         If %Found(LNP001);
           tmpDateISO = %Date(LSDT : *LongJul);
         EndIf;

       End-Proc;

       // ----------------------------------------------------------------------
       // calcPayoff : Calculate the Payoff Amount
       // ----------------------------------------------------------------------
       Dcl-Proc calcPayoff;

         Dcl-PI calcPayoff Like(LNBAL) End-PI;

         Dcl-S payOff Like(LNBAL);
         Dcl-S tmpMatDat Date(*ISO);
         Dcl-S tmpLstRenewal Date(*ISO);
         Dcl-S tmpNextMatDate Date(*ISO);
         Dcl-S isOmit Char(1);
         Dcl-S isWhatProduct Char(1);
         Dcl-S tNoteDate Date(*ISO);
         Dcl-S tPayOff Like(LNBAL);
         Dcl-S pawningDat Date(*ISO);
         Dcl-S loanRemainDays Packed(4:0);
         Dcl-S pawningDays Packed(4:0);
         Dcl-S waiverPersentage Packed(3:0);
         Dcl-S loanDays Packed(4:0);
         Dcl-S wUserCode1 Like(LNUSR1);
         Dcl-S tFutureInterest Packed(17:2);
         Dcl-S loanPeriod Packed(6:0);
         Dcl-S numberOfDateDiff Packed(6:0);
         Dcl-S perDay Packed(5:2);

         Chain (bankCode : loanAccount) LNP0031;
         If %Found(LNP003) and LNMTDT <> *Zeros;
           // Get the Maturity Date
           tmpMatDat = %Date(LNMTDT : *DMY);
           // Current Term and Principle Accrual Balance
           currentTerm = LNSCPD;
           chkCurrentTerm = LNSCPD;
           principleAccBalance = LNSPRN;
           currentSchedule = LNSCNR;

           // If tmpMatDat > tmpDateISO or tmpNextMatDate > tmpDateISO;
           wUserCode1 = *Blanks;
           wUserCode1 = LNUSR1;

           isOmit = ckeckProductOmit();
           If isOmit <> '1';
             // Get the Next Maturity Date
             If LNNXMT <> *Zero;
               tmpNextMatDate = %Date(LNNXMT : *DMY);
             EndIf;
             // Get the last renewal date
             If LNRNDT <> *Zeros;
               tmpLstRenewal = %Date(LNRNDT : *DMY);
             EndIf;
             tNoteDate = %Date(LNNTDT : *DMY);
             isWhatProduct = ckeckPawningProd();
             Select;
               When isWhatProduct = '1'; // Pawning
                 // Check the Date diffrence, Process only diffrance is less than parameter
                 pawningDays = %Diff(tmpDateISO : tNoteDate : *Days);
                 If pawningDays <= pawnCalcDays;
                   payOff = (pawnCalcDays - pawningDays) * LNACCR;
                   If payOff < *Zeros;
                     payOff = payOff * -1;
                   EndIf;
                 EndIf;

               When isWhatProduct = '2'; // Cheque and Bill Discounting
                 // Get the diffrance of today and maturity date
                 If LNRNDT <> *Zeros;
                   loanRemainDays = %Diff(tmpNextMatDate:tmpLstRenewal:*Days);
                 Else;
                   loanRemainDays = %Diff(tmpMatDat : tmpDateISO : *Days);
                 EndIf;
                 If loanRemainDays >= 1;
                   loanRemainDays += 1;
                   tPayOff = ((LNBAL*cdbdPersentage/100)/365)*loanRemainDays;
                   // Show the Minus figure for CD and BD
                   PayOff = tPayOff;
                 EndIf;

               When isWhatProduct = '3'; // Leasing
                 If LNBAL <> *Zeros;
                   loanDays = %Diff(tmpDateISO : tNoteDate : *Days);
                   loanRemainDays = %Diff(tmpMatDat : tmpDateISO : *Days);
                   If loanDays > 365;
                     If LNBLDT <> *Zeros;
                       tFutureInterest = getFutureInterest();
                       tFutureInterest = tFutureInterest + LNBLIN + LNBLPR;
                     EndIf;
                     tPayOff = (tFutureInterest * leGTPersentage)/100;
                   Else;
                     If LNBLDT <> *Zeros;
                       tFutureInterest = getFutureInterest();
                       tFutureInterest = tFutureInterest + LNBLIN + LNBLPR;
                     EndIf;
                     tPayOff = (tFutureInterest * leLEPersentage)/100;
                   EndIf;
                   payOff = tPayOff;
                   // Get the diffrance of today and maturity date
                   If LNRNDT <> *Zeros;
                     loanRemainDays = %Diff(tmpNextMatDate:tmpLstRenewal:*Days);
                   Else;
                     loanRemainDays = %Diff(tmpMatDat : tmpDateISO : *Days);
                   EndIf;
                   loanRemainDays -= 1;
                   If loanRemainDays <= *Zeros;
                     payOff = *Zeros;
                   EndIf;
                 EndIf;

               When isWhatProduct = '0'; // Other Products
                 payOff = LNBAL * payOffPersentage / 100;
                 // Get the diffrance of today and maturity date
                 If LNRNDT <> *Zeros;
                   loanRemainDays = %Diff(tmpNextMatDate:tmpLstRenewal:*Days);
                 Else;
                   loanRemainDays = %Diff(tmpMatDat : tmpDateISO : *Days);
                 EndIf;
                 If loanRemainDays <= 60;
                   payOff = loanRemainDays * LNACCR;
                 EndIf;
             EndSl;
           EndIf;

           // If the Loan is already Closed
           If LNSTAT = 'C';
             payOff = *Zeros;
           EndIf;

         EndIf;

         // If Early Payoff Penalty Exit Routine
         If wUserCode1 <> *Blanks;
           // Check for Bill and Cheque Discounting
           kPKey3 = 'USCD_' + %Trim(%EditC(LNTYPE : '4'));
           kPKey4 = wUserCode1;
           Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
           If %Found(MPARM00);
             loanRemainDays = %Diff(tmpMatDat : tmpDateISO : *Days);
             waiverPersentage = %Int(MBSVAL1);
             payOff = (LNSPRN * ((LNRATE)/365) * (loanRemainDays)
                      * waiverPersentage)/100;
           EndIf;
           // Check for other products
           kPKey3 = 'USCD_' + '9999';
           Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
           If %Found(MPARM00);
             waiverPersentage = %Int(MBSVAL1);
             payOff = LNBAL * waiverPersentage / 100;
           EndIf;
         EndIf;

         // If the Loan is already Maturead set the Payoff to Zero
         If tmpNextMatDate <= tmpDateISO;
           payOff = *Zeros;
         EndIf;

         Return payOff;

       End-Proc;

       // ----------------------------------------------------------------------
       // ckeckProductOmit : Check for product Ommision
       // ----------------------------------------------------------------------
       Dcl-Proc ckeckProductOmit;

         Dcl-PI ckeckProductOmit Char(1)  End-PI;

         Dcl-S omitProduct Char(1);

         // Documentation Charges for Products
         kPKey2 = 'LOANPAYOFF';
         kPKey3 = 'OMIT_PROD';
         kPKey4 = %Trim(%EditC(LNTYPE : '4'));
         omitProduct = '0';

         Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
         If %Found(MPARM00);
           omitProduct = '1';
         EndIf;

         Return omitProduct;

       End-Proc;

       // ----------------------------------------------------------------------
       // ckeckPawningProd : Check for Pawning Products
       // ----------------------------------------------------------------------
       Dcl-Proc ckeckPawningProd;

         Dcl-PI ckeckPawningProd Char(1)  End-PI;

         Dcl-S isWhatProduct Char(1);

         // Check for Pawning
         kPKey2 = 'LOANPAYOFF';
         kPKey3 = 'PAWNING_PR';
         kPKey4 = %Trim(%EditC(LNTYPE : '4'));
         isWhatProduct = '0';

         Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
         If %Found(MPARM00);
           isWhatProduct = '1';
           pawnCalcDays = *Zeros;
           pawnCalcDays = %Int(MBSVAL1);
         EndIf;

         // Check for Bill and Cheque Discounting
         kPKey3 = 'CDBD_PROD';
         Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
         If %Found(MPARM00);
           isWhatProduct = '2';
           cdbdPersentage = *Zeros;
           cdbdPersentage = %Int(MBSVAL1);
         EndIf;

         // Leasing Less than 1 Year
         kPKey3 = 'LEHP_*LE1';
         Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
         If %Found(MPARM00);
           isWhatProduct = '3';
           leLEPersentage = *Zeros;
           leLEPersentage = %Int(MBSVAL1);
         EndIf;

         // Leasing Grater than 1 Year
         kPKey3 = 'LEHP_*GT1';
         Chain (kPKey1 : kPKey2 : kPKey3 : kPKey4) MPARMR;
         If %Found(MPARM00);
           isWhatProduct = '3';
           leGTPersentage = *Zeros;
           leGTPersentage = %Int(MBSVAL1);
         EndIf;

         Return isWhatProduct;

       End-Proc;

       // ----------------------------------------------------------------------
       // getFutureInterest : Calculate the Future interest
       // ----------------------------------------------------------------------
       Dcl-Proc getFutureInterest;

         Dcl-PI getFutureInterest Packed(17:2)  End-PI;

         Dcl-S calcFutureInterest Packed(17:2);
         Dcl-S totalReceivable Packed(17:2);
         Dcl-S tTotalReceivable Packed(17:2);
         Dcl-S numberofTerms Packed(5:0);
         Dcl-S vatRate Packed(5:2) Inz(115);
         Dcl-S rentalAmount Like(LXPAMT);

         SetLL (bankCode : loanAccount) LNP0055;
         ReadE (bankCode : loanAccount) LNP0055;
         Dow Not %Eof(LNP005L1);
           If LXREC >= currentSchedule;
             If LXPAMT < 9999999999999;
               If LXREC = currentSchedule;
                 totalReceivable += (LXPPMT - currentTerm) * LXPAMT;
                 rentalAmount = LXPAMT;
               Else;
                 totalReceivable += LXPPMT * LXPAMT;
                 rentalAmount = LXPAMT;
               EndIf;
             Else;
               totalReceivable += rentalAmount;
             EndIf;
           EndIf;
           ReadE (bankCode : loanAccount) LNP0055;
         EndDo;

         If LNTYPE = 2001 or LNTYPE = 2005;
           tTotalReceivable = ((totalReceivable * 100) / vatRate);
         Else;
           tTotalReceivable = totalReceivable;
         EndIf;

         If totalReceivable <> *Zeros;
           calcFutureInterest = tTotalReceivable - principleAccBalance;
           If tTotalReceivable < principleAccBalance;
             principleAccBalance -= LNBLPR;
             calcFutureInterest = tTotalReceivable - principleAccBalance;
           EndIf;
           If calcFutureInterest < *Zeros;
             calcFutureInterest = calcFutureInterest * -1;
           EndIf;
         EndIf;

         Return calcFutureInterest;

       End-Proc;
 
