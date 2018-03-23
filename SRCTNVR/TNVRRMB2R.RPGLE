000100940224     H*--------------------------------------------------------------*
000200110406     FWFMBS00F  iF   E             DISK
000201110406     FWFSLB00F  O  a E             DISK    usropn
000300110405     FFISGN01l  IF   E           K DISK
000301110405     Fazorg01l  IF   E           K DISK
000302131107     d comtfp          s                   like(sgntnp) inz(166)
000303110405     d comwho          s                   like(sgnwho)
000304110405     d flo             ds             3
000305110405     d wcolli                         1
000308110406     d Wechibar        s              1
000309110406     d webbar          s              1
000310110406     d wfil            s                   like(orgfil)
000315110405     d Trul28ds      e ds                  inz
000316110405     d   I28mod      e                     inz('BAR')
000317110405      * - DS codice a barre per calcolo check-digit
000318110405     d BARcodeDS1      ds            18    inz
000319110405     d   BARcdeFLS                    3s 0 inz
000320110405     d   BARcdeLNA                    3s 0 inz
000321110405     d   BARcdeNRS                    2s 0 inz
000322110405     d   BARcdeNSC                    7s 0 inz
000323110405     d   BARcdeZNC                    2s 0 inz
000801110405     d                 ds
000802110405     d §11f                    1     11s 0
000803110405     d §11                     1     11s 0 DIM(11)
000804110405     D* RADICE + N.PROGRESSIVO CHI SONO BARTOLINI
000805110405     d sscchib         ds            35
000806110405     d  radchib                1      2
000807110405     d  prgchib                3     13s 0
000808110405     d  chkdig                14     14s 0
000809110405     d* DS per routine calcolo check digit segnacollo Bartolini (fls/lna...)
000810110405     D                 DS
000811110405     D  §CB                    1     18  0
000812110405     D                                     DIM(18)
000813110405     D  §CA                    1     18
000814110405     D                                     DIM(18)
000815110405     D                 DS
000816110405     D  §2                     1      2  0
000817110405     D                                     DIM(2)
000818110405     D  §2A                    1      2
000819110405     D                                     DIM(2)
000820110405     D digits          c                   '0123456789'
000821110405     D nn              s              2  0
000822110405     D wchkd           s              1  0
000823110406      * - Comandi per pulizia Work-Files
000824110406     d c_Cmd_ClrWF1    c                   const('clrpfm file(*libl/+
000825110406     d                                                   WFSLB00F)')
000826110406      * - Parametri per pgm. esecuzione comandi di sistema
000827110406     d Qcmd            s             80    inz
000828110406     d Qlen            s             15  5 inz(%len(Qcmd))
000900900131     C*---------------------------------------------------------------*
001600060512     c
001601110405     C     KSGN          KLIST
001602110405     C                   KFLD                    COMTFP
001603110405     C                   KFLD                    comwho
001604110406      * Pulizia work-files e loro apertura
001605110406     c                   call      'QCMDEXC'
001606110406     c                   parm      c_Cmd_ClrWF1  Qcmd
001607110406     c                   parm                    Qlen
001608110406     c                   open      WFSLB00F
001700060512
001701110406     c                   clear                   wtot              2 0
001702110406     c                   clear                   savprg            6 0
001900060517    1C                   DO        *hival
002000110405     c                   read      wfmbs00f
002100110405    2c                   if        %eof(wfmbs00f)
002200060517     c                   leave
002300060517    2c                   endif
002301110406     c* a rottura progressivo se > 0 scrivo file per statistica
002302110406     c                   if        §mbsprg<>savprg
002303110406     c                   if        wtot>0
002304110406     c                   exsr      sr_wslb
002314110406     c                   endif
002315110406     c                   eval      savprg=§mbsprg
002316110406     c                   clear                   wtot
002317110406     c                   clear                   n_wcolli          2 0
002318110406     c                   clear                   n_Wchibar         2 0
002319110406     c                   clear                   n_Wchibarc        2 0
002320110406     c                   clear                   n_wbbar           2 0
002321110418     c                   clear                   n_wbbas           2 0
002322110406     c                   endif
002323110406     c                   clear                   wcolli
002324110405     c* verifico se esiste su file colli
002325110405     c                   movel(P)  §mbsbrcd      comwho
002326110405     c     ksgn          setll     fisgn01l
002327110405     c                   if        %equal(fisgn01l)
002328110406     c                   add       1             n_wcolli
002329110406     c                   eval      wcolli='S'
002330110405     c                   endif
002331110405     c* verifico se si tratta di barcode bartolini  (FLS/LNA..)
002332110405     c                   exsr      sr_bar_bar
002333110405     c                   if        webbar=*blanks
002334110406     c                   add       1             n_wbbar
002335110418     c                   if        %subst(comwho:7:2)>'00'
002336110418     c                   add       1             n_wbbas
002337110418     c                   endif
002338110405     c                   else
002339110405     c* verifico se si tratta di chi sono bartolini (AA+......)
002340110405     c                   exsr      sr_chi_bar
002341110405     c                   if        wechibar=*blanks
002342110406     c                   add       1             N_wchibar
002343110406     c                   if        wcolli='S'
002344110406     c                   add       1             n_wchibarc
002345110406     c                   endif
002346110405     c                   endif
002347110405     c                   endif
002348110406     c                   add       1             wtot
005000060512      *
005100060517    1C                   ENDDO
005101110406      *
005102110406     c                   if        wtot>0
005103110406     c                   exsr      sr_wslb
005104110406     c                   endif
005200030205      *
005300000000     C                   SETON                                        LR
005301110405     c****************************************************
005302110405     C     sr_bar_bar    begsr
005303110405     c                   clear                   webbar
005305110405     c     digits        check     comwho        nn
005306110405    2c     nn            ifne      19
005307110405     c                   eval      webbar='N'
005308110405     c                   leavesr
005309110405     c                   else
005310110405     c     ' '           check     comwho:19     nn
005311110405    2c     nn            ifgt      *zeros
005312110405     c                   eval      webbar='N'
005313110405     c                   leavesr
005314110405     c                   endif
005315110405     c                   endif
005316110405     c* primi 6 bytes --> fls/LNA: devono esistere su azorg
005317110405     c* FLS
005318110405     c                   movel     comwho        wfil
005319110405     c                   exsr      sr_azorg
005320110405     c                   if        webbar='N'
005321110405     c                   leavesr
005322110405     c                   endif
005323110405     c* LNA
005326110405     c                   movel     comwho        w006a             6
005327110405     c                   move      w006a         wfil
005328110405     c                   exsr      sr_azorg
005329110405     c                   if        webbar='N'
005330110405     c                   leavesr
005331110405     c                   endif
005332110405     c* controllo il check digit
005333110405     c                   movel     comwho        barcodeds1
005334110405     c                   reset                   TRUL28ds
005335110405     c                   movel     BarCodeDS1    I28cod
005336110405     c                   call      'TRUL28R1'
005337110405     c                   parm                    TRUL28DS
005338110405     c                   if        o28err<>*blanks or
005339110405     c                             o28ckd<>%subst(comwho:18:1)
005340110405     c                   eval      webbar='N'
005341110405     c                   leavesr
005342110405     c                   endif
005343110405     c
005344110405     c*
005345110405     c                   endsr
005346110405     c****************************************************
005347110405     C     sr_chi_bar    begsr
005348110405     c                   clear                   wechibar
005349110405     c* CHI SONO BARTOLINI: AA/CC+12 NUMERI
005350110405     c                   if        %subst(comwho:1:2)<>'AA' and
005351110405     c                             %subst(comwho:1:2)<>'CC'
005352110405     c                   eval      wechibar='N'
005353110405     c                   leavesr
005354110405     c                   endif
005355110405     c* dalla posizione 3 alla posizione 14 devono essere tutti numeri
005356110405     c* e il resto (da 15 a 35) devono essere tutti blank
005357110405     c     digits        check     comwho:3      nn
005358110405    2c     nn            ifne      15
005359110405     c                   eval      wechibar='N'
005360110405     c                   leavesr
005361110405     c                   endif
005362110405     c     ' '           check     comwho:15     nn
005363110405    2c     nn            ifgt      *zeros
005364110405     c                   eval      wechibar='N'
005365110405     c                   leavesr
005366110405     c                   endif
005367110405     c* controllo il check digit
005368110405     c                   movel     comwho        sscchib
005369110405     c                   move      prgchib       §11f
005370110405     c                   exsr      m1031
005371110405    2c     chkdig        ifne      wchkd
005372110405     c                   eval      wechibar='N'
005373110405     c                   leavesr
005374110405     c                   endif
005375110405     c
005376110405     c                   endsr
005377110405     c*
005378110405     c     sr_azorg      begsr
005379110405     c     wfil          chain     azorg01l
005380110405     c                   if        not %found(azorg01l) or
005381110405     c                             orgfva <> *blanks or (orgfag<>'F' and
005382110405     c                             orgfag<>'A')
005383110405     c                   eval      webbar='N'
005384110405     c                   endif
005385110405     c                   endsr
005400110405      ** CALCOLA CHECK DIGIT -----------------------------
005500110405     C     m1031         begsr
005600110405      *
005700110405     C                   move      *zeros        §TT2              3 0
005800110405     C                   clear                   w0030             3 0
005900110405     C                   clear                   w0010             1 0
006000110405      *
006100110405      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
006200110405     C                   do        11            xx                2 0
006300110405      *
006400110405     C     xx            div       2             §ctr              3 0
006500110405     C                   mvr                     §RST              3 030
006600110405      *
006700110405     C     *IN30         IFEQ      '1'
006800110405     C     §11(XX)       MULT      3             §CTR
006900110405     C                   ELSE
007000110405     C                   Z-ADD     §11(XX)       §CTR
007100110405     C                   ENDIF
007200110405      *
007300110405     C                   ADD       §CTR          W0030
007400110405     C                   ENDDO
007500110405      *
007600110405     C     W0030         ADD       9             §TT2
007700110405     C                   MOVE      0             §TT2
007800110405     C     §TT2          SUB       W0030         wchkd
007900110405      *
008000110405     C                   ENDSR
008001110406     c************************************************************
008002110406     c     sr_wslb       begsr
008100110406     c                   clear                   wfslb000
008200110406     c                   eval      slbprg=savprg
008300110406     c                   eval      SLBBRCC=n_wcolli
008400110406     c                   eval      SLBCHIB=n_wchibar
008500110406     c                   eval      SLBCHIBC=n_wchibarc
008600110406     c                   eval      SLBBBAR=n_wbbar
008601110418     c                   eval      SLBBBAS=n_wbbas
008700110406     c                   z-add     wtot          slbbrct
008800110406     c                   write     wfslb000
008900110406     c                   endsr
