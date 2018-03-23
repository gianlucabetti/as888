000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090504      **
000300090504     H* TNVRA25R *---------------------------------------------------*
000400090424      **
000500090424     C*=================================================================
000600090504      *
000700090505     F****fitgt01L  uF   E           K DISK
000800090505     Ffifgt01L  iF   E           K DISK
000900090505     FfifgtSAV  O  A E             DISK      rename(fifgt000:fgtsav)
001000090505     Ffifpt01L  iF   E           K DISK
001100090505     FfifptSAV  O  A E             DISK      rename(fifpt000:fptsav)
001200090505     Ffifpd01L  iF   E           K DISK
001300090505     FfifpdSAV  O  A E             DISK      rename(fifpd000:fpdsav)
001400090504      *
001500090504     FwxFGT00S  o  a E             DISK
001600090504      *
001700081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001800081211      * -------------------------------------------------------------
001900090504     D dsTGT         E DS                  ExtName(FITGT00F)
002000081110     d                                     prefix(STP:3)
002100081110     D esito           s             10i 0
002200090112     C*****************************************************************
002300081209     C                   SETON                                        LR
002400090424     C*=================================================================
002500090505     C/EXEC SQL
002600090505     C+  delete  from wxfgt00s where WSFPROGR = 'TNVRA25R'
002700090505     C/END-EXEC
002800090505
002900090505
003000090422     C/EXEC SQL DECLARE A1 CURSOR FOR
003100090504     c+   select * from fitgt00f where tgtpdr in (
003200090504     C+   select fgtpdr from edpba/fgt999)
003300090504     C+   ORDER BY tgtSML, TGTPDR, TGTPRG
003400090422     C/END-EXEC
003500081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
003600081110     c                   if        sqlCODE < 0
003700081110     c                   eval      esito = Sqlcode
003800081110     c                   dump(a)
003900081110     c                   return
004000081110     c                   end
004100081110     C/EXEC SQL
004200081110     C+ OPEN A1
004300081110     C/END-EXEC
004400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
004500081110     c                   if        sqlCODE < 0
004600081110     c                   eval      esito = Sqlcode
004700081110     c                   dump(a)
004800081110     c                   return
004900081110     c                   else
005000081110     c                   except    testa
005100081110     c                   end
005200090109      * >>>>>>>
005300081209      * resto nel ciclo finchè....
005400081110     C                   DOU       SqlCod < 0
005500081110      * lettura cursore
005600081110     C/EXEC SQL
005700090504     C+ FETCH NEXT FROM A1 INTO :dsTGT
005800081110     C/END-EXEC
005900081110      *  EoF
006000081110     C                   if        SqlCod = 100
006100081110     c                   Leave
006200081110      *  x errori
006300081110     C                   ElseIf    SqlCod < 0
006400081110     c                   eval      esito = Sqlcode
006500081110     c                   dump(a)
006600081110     c                   leave
006700081110     C                   ENDif
006800081110      *
006900090109     c                   clear                   eseguito          1
007000090109     c                   clear                   verifica          1
007100090102     c                   clear                   errore           10
007200090109      *
007300090109      *  Solo in aggiornamento
007400090424      *  Deve Cancellare testata e Dettagli
007500081110     c                   if        Aggiorna = 'S'
007600090504      *
007700090504      *
007800090504     c                   z-add     0             CONTA             3 0
007900090504     c                   z-add     0             AUTFGT            7 0
008000090504     C/EXEC SQL
008100090504     C+  select fgtpdr, count(*) INTO :AUTfgt, :conta
008200090504     C+  from
008300090504     C+   (SELECT fgtpdr, fgttsr
008400090504     C+  FROM edpab/svfgtrif WHERE  fgtatb=' ' and
008500090504     C+  Fgtsml=' ' and fgtdst>20090501 and
008600090504     C+  fgttsr in ('C', 'R', 'G') and fgtpdr = :stpPDR
008700090504     C+  group by fgtpdr, fgttsr ) as a
008800090504     C+  group by fgtpdr
008900090504     C/END-EXEC
009000090504      *
009100090504      *  scrive il dettaglio su un file
009200090504     c                   exsr      wri_DETTA
009300090504      *
009400090505     c                   if        Conta > 1
009500090504      *
009600090504      *  se invece sono più di 1 riga di tariffa occorre scrivere quelle
009700090504      *   che non rientravano nella decorrenza di aprile ossia quelle antecedenti
009800090504      *
009900090504     c     kfGT01        setll     fifgt01l
010000090504     c     kfGT01        reade     fifgt01l
010100090504     c                   dow       not %Eof(fifgt01l)
010200090504      **
010300090504      **  quelle che erano state annullate
010400090505     c                   if        fgtddt < 20090401 and fgtdst = 20090430
010500090505     C                   CLEAR                   DATASCAD          8 0
010600090505     C/EXEC SQL
010700090505     C+ SELECT fgtDST INTO :DATASCAD FROM edpab/svfgtrif WHERE fgtatb=' '
010800090505     C+ and Fgtsml=' ' and FGTPDR= :FGTPDR AND FGTTSR =:FGTTSR AND
010900090505     C+ FGTCTR=:FGTCTR AND FGTPRG=:FGTPRG
011000090505     C/END-EXEC
011100090505     c                   if        DATASCAD <> 20090430
011200090505      **
011300090504      *------------------
011400090504      *  genera
011500090504      *     i dettagli con il nuovo progressivo
011600090504     c                   exsr      FPT_FPD
011700090504      *------------------
011800090504      *  genera
011900090504      *       il nuovo FGT allineato alla supertestata
012000090505     c                   eval      fgtDST = stpDST
012100090504     c                   eval      fgtDDT = 20090501
012200090504     c                   z-add     200           fgtPRG
012300090504     c                   z-add     *date         fgtDUV
012400090504     c                   clear                   fgtDCN
012500090504     c                   clear                   fgtFTR
012600090504     c                   clear                   fgtDTR
012700090504      *
012800090505     c                   write     fgtSAV
012900090504     c                   endif
013000090504      **
013100090505     c                   endif
013200090505      **
013300090504     c     kfGT01        reade     fifgt01l
013400090504     c                   enddo
013500090504      *
013600090504     c                   endif
013700090504      *
013800090505      *  se è se stesso allora rilascio da stampare
013900090505      *
014000090505      * Rilascia il TGT da stampare
014100090505      ******
014200090505     c**** kTGT01        chain     fitgt01l
014300090505     c*****              if        %Found(fitgt01l)
014400090505     c*****              if        conta = 1 and tgtdcn < 20391231
014500090505     c*****                        and  tgtdcn > 0
014600090505     c*****              else
014700090505     c*****              clear                   tgtdcn
014800090505     c*****              endif
014900090505      *****
015000090505     c*****              update    fitgt000
015100090505     c*****              endif
015200090505      *****
015300090504     c                   endif
015400090109      *------------------
015500081210      *  se sono qui ho letto un record correttamente
015600081210     c   Of              except    testa
015700081210     c                   except    detta
015800081110      *
015900081110     C                   ENDDO
016000081110     C/EXEC SQL
016100081110     C+ Close A1
016200081110     C/END-EXEC
016300081110     C                   if        Esito < 0
016400081110     c                   return
016500081110     c                   endIF
016600090424     C*
016700081110     C     FINE          TAG
016800081110     c                   except    finest
016900081110     c                   return
017000081209     C*------------------------------------------------------------------------*
017100081209     C* *INZSR - OPERAZIONI INIZIALI
017200081209     C*------------------------------------------------------------------------*
017300081209     C     *INZSR        BEGSR
017400081209      *
017500081209     C     *ENTRY        PLIST
017600081209     C                   PARM                    AGGIORNA          1
017700090109      *
017800090504     c     kTGT01        klist
017900090504     c                   kfld                    stpPDR
018000090504     c                   kfld                    stpSML
018100090504     c                   kfld                    stpPRG
018200090504      *
018300090504     c     kFGT01        klist
018400090504     c                   kfld                    stpPDR
018500090504     c                   kfld                    stpSML
018600090504      *
018700090504      * duplica le tariffe FPT e FPD
018800090504     c     Key_FPT       KLIST
018900090504     c                   kfld                    fgtPDR
019000090504     c                   kfld                    fgtSML
019100090504     c                   kfld                    fgtTSR
019200090504     c                   kfld                    fgtCTR
019300090504     c                   kfld                    fgtPRG
019400081209     C*
019500081209     c                   clear                   esito
019600081209      *
019700081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
019800081209     C                   clear                   UDATE_8           8 0
019900081209     C                   MOVEL     *DATE         UDATE_8
020000090109      *
020100081209     c                   endSR
020200090504     C*---------------------------------------------------------------*
020300090504      *  Scrive il Dettaglio che sta stampando x il controllo finale
020400090504      *---------------------------------------------------------------*
020500090504     c     WRI_DETTA     begSR
020600090504     C*
020700090504     c                   clear                   WSFGT000
020800090504     C*
020900090504     c                   eval      WSFSML  =  stpSML
021000090504     c                   eval      WSFPDR  =  stpPDR
021100090504     c                   eval      WSFPRG  = stpPRG
021200090504     c                   eval      WSFDDT  = stpDDT
021300090504     c                   eval      WSFDST  = stpDST
021400090504     c                   eval      WSFDTS  = stpDTS
021500090504     c                   eval      WSFDCN  = stpDCN
021600090504     c                   eval      WSFREC  = conta
021700090504     c                   eval      WSFPROGR= 'TNVRA25R'
021800090504     c**
021900090504     c                   movel     conta         WSFNOTE
022000090504     c                   eval      WSFNOTE = %trim(Wsfnote) + '- TSR attivi'
022100090505     c**
022200090505     c**  Quelle da non ristampare
022300090505     c                   if        conta = 1 and stpdcn < 20391231
022400090505     c                             and  stpdcn > 0
022500090505     c                   movel(p)  conta         WSFNOTE
022600090505     c                   eval      WSFNOTE = %trim(Wsfnote) + '- NON ristampata'
022700090505     c                   eval      wsfTSR = '*'
022800090505     c                   end
022900090504     c**
023000090504     c                   write     WSFGT000
023100090504     C*
023200090504     c                   endSR
023300090504      *---------------------------------------------------------------*
023400090504      *  Scrive Testate e Dettaglio
023500090504      *---------------------------------------------------------------*
023600090504     c     FPT_FPD       begSR
023700090504     C*
023800090504     c     Key_FPT       setll     fifpt01L
023900090504     c     Key_FPT       reade     fifpt01L
024000090504     c                   Dow       not %EoF(fifpt01l)
024100090504     C*
024200090504     c                   z-add     200           fptPRG
024300090504     C*
024400090505     c                   write     fptSAV
024500090504      *
024600090504     c     Key_FPT       reade     fifpt01L
024700090504     c                   endDo
024800090504     C*
024900090504     c     Key_FPT       setll     fifpd01L
025000090504     c     Key_FPT       reade     fifpd01L
025100090504     c                   Dow       not %EoF(fifpd01l)
025200090504      *
025300090504     c                   z-add     200           fpdPRG
025400090505     c                   write     fpdSAV
025500090504      *
025600090504     c     Key_FPT       reade     fifpd01L
025700090504     c                   endDo
025800090504     C*
025900090504     c                   endSR
026000090504     C*---------------------------------------------------------------*
026100081110     OQSYSPRT   E            TESTA            02
026200081110     O                                           16 'Simul'
026300081110     O                                           25 'Autista'
026400081110     O                                           45 'Progr'
026500081110     O                                           55 'Data_Dec'
026600081110     O                                           65 'Data_Sca'
026700090504     O                                           75 'Data_Cnv'
026800090504     O                                           85 'Data_Stp'
026900081110     O          E            DETta       1
027000081110     O                       stpSML              14
027100081110     O                       stpPDR        z     25
027200081110     O                       stpPRG        z     43
027300081110     O                       stpDDT        z     55
027400081110     O                       stpDST        z     65
027500081110     O                       stpDTS        z     75
027600081110     O                       stpDCN        z     85
027700090504     o                       wsfNote            132
027800081110      ****
027900081110     o          E            FINEST      2
028000090504     o                                           30 '*** TNVRA25R ***'
028100090102     o                                           59 '***   FINE  STAMPA  ***'
028200090504     o                                          + 9 '***  DI  CONTROLLO  ***'
