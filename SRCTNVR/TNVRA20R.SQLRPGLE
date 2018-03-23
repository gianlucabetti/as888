000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090424     H* TNVRA20R *---------------------------------------------------*
000300090424      **
000400090424     C*=================================================================
000500090424      **      DEVE GIRARE prima della creazione delle SUPERTESTATE
000600090424     C*=================================================================
000700090424      **
000800090424     H* TNVRA20R *---------------------------------------------------*
000900081110     H*                                                              *
001000090424     H* questo programma deve eliminare le tariffe con date          *
001100090424     H*     DECORRENZA      SCADENZA     sullo stesso record         *
001200090424     H*        1/5    2009    1/5                                    *
001300090424     H*        1/5    2009    2/5                                    *
001400090424     H*        1/5    2009    3/5                                    *
001500090424     H*        2/5    2009    2/5                                    *
001600090424     H*        2/5    2009    3/5                                    *
001700090424     H*        3/5    2009    3/5                                    *
001800090424     H*                                                              *
001900000000     H*--------------------------------------------------------------*
002000090424     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
002100090109      *
002200090424     Ffifpt01L  uF   E           K DISK
002300090424     Ffifpd01L  uF   E           K DISK
002400081211      *
002500090424     FwxFGT00S  o  a E             DISK
002600081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002700081211      * -------------------------------------------------------------
002800081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002900081110     d                                     prefix(STP:3)
003000081110     D esito           s             10i 0
003100081110     D FGT_REC         s              9s 0
003200090424     D PERIODO         s              8s 0
003300090112     C*****************************************************************
003400081209     C                   SETON                                        LR
003500090422     C*=================================================================
003600090423      **  Ricordiamo che:
003700090423      **    PRIMA sono state fatte scadere al 30/04/2009 tutte le altre
003800090423      *      che non devono essere CONVERTITE.
003900090422      **
004000090424      **   Con questo Programma deve ELIMINARE tutte le tariffe inserite
004100090424      **   Erroneamente fra il 1/5 ed il 3/5
004200090424      **
004300090424     C*=================================================================
004400090424      **      DEVE GIRARE prima della creazione del TGT
004500090423     C*=================================================================
004600090424      *  esempio:
004700090424      *  COD       FL.SIMULAZ.  TIPO   COD  PRG  DIVISA       DATA         DATA     SCADENZA -
004800090424      *  AUTOTRAS  TARIFFA      PREST  TAR  TAR  TARIFFA      DECOR.       SCAD.    DECORRENZA
004900090424      *                         PADR                          TAR.         TAR.     IN GIORNI
005000090424      *    220126                 C    300    3    EUR    2009/05/01  2009/05/01       0
005100090424      *    220154                 C    300    2    EUR    2009/05/02  2009/05/02       0
005200090424      *    360005                 G    999    2    EUR    2009/05/01  2009/05/01       0
005300090424      *    360055                 G    999    1    EUR    2009/05/01  2009/05/01       0
005400090424      *    360059                 G    999    5    EUR    2009/05/01  2009/05/02       1
005500090424      *    360068                 G    999    1    EUR    2009/05/01  2009/05/01       0
005600090424      *    360071                 G    999    2    EUR    2009/05/01  2009/05/02       1
005700090424      *    360081                 G    999    2    EUR    2009/05/01  2009/05/01       0
005800090424      *    360081                 G    999    3    EUR    2009/05/02  2009/05/02       0
005900090424      *    710619                 C    300    1    EUR    2009/05/01  2009/05/01       0
006000090424      *    710619                 C    300    2    EUR    2009/05/02  2009/05/02       0
006100090424      *    710619                 C    300    3    EUR    2009/05/03  2009/05/03       0
006200090424      *    710619                 G    999    2    EUR    2009/05/01  2009/05/01       0
006300090424      *    710619                 G    999    3    EUR    2009/05/02  2009/05/02       0
006400090424      *    710619                 R    300    1    EUR    2009/05/01  2009/05/01       0
006500090424      *    710619                 R    300    2    EUR    2009/05/02  2009/05/02       0
006600090424      *    830020                 C    300    1    EUR    2009/05/01  2009/05/01       0
006700090424      *    830020                 R    300    1    EUR    2009/05/01  2009/05/01       0
006800090424      *    830038                 G    999    1    EUR    2009/05/01  2009/05/02       1
006900090424      *    890483                 G    999    1    EUR    2009/05/01  2009/05/01       0
007000090424      *A  2490675                 G    999    1    EUR    2009/05/01  2009/05/01       0
007100090424      *
007200090422     C/EXEC SQL DECLARE A1 CURSOR FOR
007300090424     C+   Select * from (
007400090424     C+   SELECT (FGTdst - FGTddt) as Periodo,
007500090424     C+   a.* , rrn(a) FROM
007600090424     C+   fifgt00f as a
007700090424     C+   WHERE  fgttsr in ('C', 'R', 'G')
007800090424     C+   and fgtddt between 20090501 and 20090509
007900090424     C+   and fgtdst between 20090501 and 20090509
008000090424     C+     )
008100090424     C+   as daEliminare
008200090424     C+   where Periodo between  0 and 4
008300090424     C+   ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
008400090422     C/END-EXEC
008500081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
008600081110     c                   if        sqlCODE < 0
008700081110     c                   eval      esito = Sqlcode
008800081110     c                   dump(a)
008900081110     c                   return
009000081110     c                   end
009100081110     C/EXEC SQL
009200081110     C+ OPEN A1
009300081110     C/END-EXEC
009400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009500081110     c                   if        sqlCODE < 0
009600081110     c                   eval      esito = Sqlcode
009700081110     c                   dump(a)
009800081110     c                   return
009900081110     c                   else
010000081110     c                   except    testa
010100081110     c                   end
010200090109      * >>>>>>>
010300081209      * resto nel ciclo finchè....
010400081110     C                   DOU       SqlCod < 0
010500081110      * lettura cursore
010600081110     C/EXEC SQL
010700090424     C+ FETCH NEXT FROM A1 INTO :PERIODO, :dsFGT, :FGT_REC
010800081110     C/END-EXEC
010900081110      *  EoF
011000081110     C                   if        SqlCod = 100
011100081110     c                   Leave
011200081110      *  x errori
011300081110     C                   ElseIf    SqlCod < 0
011400081110     c                   eval      esito = Sqlcode
011500081110     c                   dump(a)
011600081110     c                   leave
011700081110     C                   ENDif
011800081110      *
011900090109     c                   clear                   eseguito          1
012000090109     c                   clear                   verifica          1
012100090102     c                   clear                   errore           10
012200090109      *
012300090109      *  Solo in aggiornamento
012400090424      *  Deve Cancellare testata e Dettagli
012500081110     c                   if        Aggiorna = 'S'
012600090424      *  cancella i dettagli
012700090423     c                   exsr      FPT_FPD
012800090109      *
012900090424      *  poi la testata dell'FGT
013000081218     c     fgt_REC       chain     FIFGT00F
013100081218     c                   if        %Found(FIFGT00F)
013200090424     c                   delete    fgtFisico
013300090109     c                   endif
013400090424      *
013500090424     c                   endif
013600090109      *------------------
013700081210      *  se sono qui ho letto un record correttamente
013800081210     c   Of              except    testa
013900081210     c                   except    detta
014000090424      *
014100090424      *  scrive il dettaglio su un file
014200090424     c                   exsr      wri_DETTA
014300081110      *
014400081110     C                   ENDDO
014500081110     C/EXEC SQL
014600081110     C+ Close A1
014700081110     C/END-EXEC
014800081110     C                   if        Esito < 0
014900081110     c                   return
015000081110     c                   endIF
015100081110     C*
015200090424     C* DEVE ELIMINARE i RECORD ANNULLATI dell'FGT
015300090424     C/EXEC SQL
015400090424     C+ delete   from fifgt00f where fgtatb='A'
015500090424     C/END-EXEC
015600090424     C*
015700090424     C/EXEC SQL
015800090424     C+ delete   from fifpt00f where fptatb='A'
015900090424     C/END-EXEC
016000090424     C*
016100090424     C/EXEC SQL
016200090424     C+ delete   from fifpd00f where fpdatb='A'
016300090424     C/END-EXEC
016400090424     C*
016500081110     C     FINE          TAG
016600081110     c                   except    finest
016700081110     c                   return
016800081209     C*------------------------------------------------------------------------*
016900081209     C* *INZSR - OPERAZIONI INIZIALI
017000081209     C*------------------------------------------------------------------------*
017100081209     C     *INZSR        BEGSR
017200081209      *
017300081209     C     *ENTRY        PLIST
017400081209     C                   PARM                    AGGIORNA          1
017500090109      *
017600090109      * duplica le tariffe FPT e FPD
017700090109     c     Key_FPT       KLIST
017800090109     c                   kfld                    stpPDR
017900090109     c                   kfld                    stpSML
018000090109     c                   kfld                    stpTSR
018100090109     c                   kfld                    stpCTR
018200090109     c                   kfld                    stpPRG
018300081209     C*
018400081209     c                   clear                   esito
018500081209      *
018600081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
018700081209     C                   clear                   UDATE_8           8 0
018800081209     C                   MOVEL     *DATE         UDATE_8
018900090109      *
019000081209     c                   endSR
019100090109      *---------------------------------------------------------------*
019200090109      *  Scrive Testate e Dettaglio
019300090109      *---------------------------------------------------------------*
019400090423     c     FPT_FPD       begSR
019500090109     C*
019600090109     c     Key_FPT       setll     fifpt01L
019700090109     c     Key_FPT       reade     fifpt01L
019800090109     c                   Dow       not %EoF(fifpt01l)
019900090424     c                   delete    fifpt000
020000090109     c     Key_FPT       reade     fifpt01L
020100090109     c                   endDo
020200090109     C*
020300090109     c     Key_FPT       setll     fifpd01L
020400090109     c     Key_FPT       reade     fifpd01L
020500090109     c                   Dow       not %EoF(fifpd01l)
020600090424     c                   delete    fifpd000
020700090109     c     Key_FPT       reade     fifpd01L
020800090109     c                   endDo
020900090109     C*
021000090109     c                   endSR
021100081211     C*---------------------------------------------------------------*
021200090424      *  Scrive il Dettaglio che sta stampando x il controllo finale
021300090424      *---------------------------------------------------------------*
021400090424     c     WRI_DETTA     begSR
021500090424     C*
021600090424     c                   eval      WSFSML  =  stpSML
021700090424     c                   eval      WSFPDR  =  stpPDR
021800090424     c                   eval      WSFTSR  =  stpTSR
021900090424     c                   eval      WSFCTR  =  stpCTR
022000090424     c                   eval      WSFPRG  = stpPRG
022100090424     c                   eval      WSFDDT  = stpDDT
022200090424     c                   eval      WSFDST  = stpDST
022300090424     c                   eval      WSFDTS  = stpDTS
022400090424     c                   eval      WSFDCN  = stpDCN
022500090424     c                   eval      WSFREC  = FGT_REC
022600090424     c                   eval      WSFDECOR= 0
022700090424     c                   eval      WSFSCAD = 0
022800090424     c                   eval      WSFPROGR= 'TNVRA20R'
022900090424     c**
023000090424     c                   write     WSFGT000
023100090424     C*
023200090424     c                   endSR
023300090424     C*---------------------------------------------------------------*
023400081110     OQSYSPRT   E            TESTA            02
023500081110     O                                           16 'Simul'
023600081110     O                                           25 'Autista'
023700081110     O                                           30 'TSR'
023800081110     O                                           38 'Tariffa'
023900081110     O                                           45 'Progr'
024000081110     O                                           55 'Data_Dec'
024100081110     O                                           65 'Data_Sca'
024200081110     O                                           75 'Data_Stp'
024300081110     O                                           85 'Data_Cnv'
024400081211     O                                          115 'N_Rec_FGT'
024500081110     O          E            DETta       1
024600081110     O                       stpSML              14
024700081110     O                       stpPDR        z     25
024800081110     O                       stpTSR              29
024900081110     O                       stpCTR              36
025000081110     O                       stpPRG        z     43
025100081110     O                       stpDDT        z     55
025200081110     O                       stpDST        z     65
025300081110     O                       stpDTS        z     75
025400081110     O                       stpDCN        z     85
025500081211     o                       FGT_REC       z    115
025600081110      ****
025700081110     o          E            FINEST      2
025800090424     o                                           30 '*** TNVRA20R ***'
025900090102     o                                           59 '***   FINE  STAMPA  ***'
026000090109     o                                          + 9 '***  SUPER TARIFFA  ***'
