000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200081110     H* TNVR1AR *----------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800081218     Ffifgt01L  iF   E           K DISK
000900090102      *
001000090102     Ffifpt01L  uF   E           K DISK    usropn
001100090102     Ffifpd01L  uF   E           K DISK    usropn
001200081218     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
001300081222     F                                     usropn
001400081211      *
001500081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600081211      * -------------------------------------------------------------
001700081110     D dsFGT         E DS                  ExtName(FIFGT00F)
001800081110     d                                     prefix(STP:3)
001900081110     D elimina_riferimento...
002000081110     D                 s              2a
002100081110     D esito           s             10i 0
002200081110     D FGT_REC         s              9s 0
002300931202     D KPJBA         E DS
002400081230      *
002500081230     D returnDDT       s                   like(FgtDDT)
002600081230     D returnDST       s                   like(FgtDST)
002700081230      *
002800900521     C*****************************************************************
002900081209     C                   SETON                                        LR
003000020418     C*
003100081110     C/EXEC SQL DECLARE A1 CURSOR FOR
003200090102     C+  SELECT fifgt00f.*, rrn(fifgt00f) FROM fifgt00f WHERE FGTTSR in
003300090102     C+  ('R', 'C', 'G')
003400090102     C+  and fgtpdr * 1000 + fgtctr
003500090102     C+  not in (select fgtrap * 1000 + fgtrct
003600090102     C+  from fifgt00f) ORDER BY FGTSML,
003700090102     C+  FGTPDR, FGTTSR, FGTCTR, FGTPRG
003800081110     C/END-EXEC
003900090102     C*
004000090102     *** SELECT fifgt00f.*, rrn(fifgt00f) FROM fifgt00f
004100090102     *** WHERE FGTTSR in ('R', 'C', 'G') and fgtpdr
004200090102     *** not in (select fttpdr from fiftt00f) and
004300090102     *** fgtpdr * 1000 + fgtctr
004400090102     *** not in (select fgtrap * 1000 + fgtrct
004500090102     *** from fifgt00f) ORDER BY FGTSML,
004600090102     *** FGTPDR, FGTTSR, FGTCTR, FGTPRG
004700090102     C*
004800090102     ***Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
004900081110     c                   if        sqlCODE < 0
005000081110     c                   eval      esito = Sqlcode
005100081110     c                   dump(a)
005200081110     c                   return
005300081110     c                   end
005400090102      ***
005500090102      ***
005600090102      ***
005700090102      *** SELECT fifgt00f.*, rrn(fifgt00f) FROM fifgt00f WHERE FGTTSR in
005800090102      *** ('R', 'C', 'G') and fgtpdr
005900090102      *** in (select fttpdr from (
006000090102      *** SELECT fttpdr, max(fttddc) FROM fiftt00f GROUP BY fttpdr HAVING
006100090102      *** max(fttddc) < 20081031 ) as a )
006200090102      ***  and
006300090102      *** fgtpdr * 1000 + fgtctr
006400090102      *** not in (select fgtrap * 1000 + fgtrct
006500090102      *** from fifgt00f) ORDER BY FGTSML,
006600090102      *** FGTPDR, FGTTSR, FGTCTR, FGTPRG
006700090102      ***
006800090102      ***
006900090102      ***
007000081110     C/EXEC SQL
007100081110     C+ OPEN A1
007200081110     C/END-EXEC
007300081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
007400081110     c                   if        sqlCODE < 0
007500081110     c                   eval      esito = Sqlcode
007600081110     c                   dump(a)
007700081110     c                   return
007800081110     c                   else
007900081110     c                   except    testa
008000081110     c                   end
008100081110      *
008200081209      * resto nel ciclo finchè....
008300081110      *
008400081110     C                   DOU       SqlCod < 0
008500081110      *
008600081110      * lettura cursore
008700081110     C/EXEC SQL
008800090102     C+ Fetch Next From A1 Into   :dsFGT, :FGT_REC
008900081110     C/END-EXEC
009000081110      *  EoF
009100081110     C                   if        SqlCod = 100
009200081110     c                   Leave
009300081110      *  x errori
009400081110     C                   ElseIf    SqlCod < 0
009500081110     c                   eval      esito = Sqlcode
009600081110     c                   dump(a)
009700081110     c                   leave
009800081110     C                   ENDif
009900081110      *
010000081210     c                   clear                   eseguito
010100081222     c                   clear                   verifica
010200081230     c                   clear                   errore            9
010300081110      *
010400081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
010500081110     c                   if        Aggiorna = 'S'
010600081110      *
010700090102      *  imposta il file di work a pro-memoria delle tariffe
010800081216     c                   clear                   wffgt000
010900081217     c                   z-add     STPPDR        wfTPDR
011000081217     c                   move      STPSML        wfTSML
011100081217     c                   move      STPTSR        wfTTSR
011200081217     c                   move      STPCTR        wfTCTR
011300081217     c                   z-add     STPPRG        wfTPRG
011400081217     c                   z-add     STPRAP        wfTRAP
011500081217     c                   move      STPRCT        wfTRCT
011600081216      *
011700081211      *
011800081110     C     fgt01K        chain     fifgt01l
011900081211     c                   if        %Found(fifgt01l)
012000081230      *
012100081230     c                   clear                   returnDST
012200081121      *
012300081222      *
012400090102     c                   end
012500081222      *
012600081222     c                   write     wffgt000
012700081121      *
012800081121      *  Solo se non ci sono stati errori:
012900081218      *   deve eliminare i puntamenti del codice di riferimento
013000081230     c*******            if        eseguito = *blank
013100081222      *
013200081222     c                   open      fifgt00f
013300081218     c     fgt_REC       chain     FIFGT00F
013400081218     c                   if        %Found(FIFGT00F)
013500090102      *
013600090102      *  Tariffa da Annullare
013700090102     c                   move      '*X'          fgtDCV
013800090102     c                   endIf
013900081230      *
014000081218     c                   update    fgtFisico
014100081121     c                   endif
014200081230      *
014300081222     c                   close     fifgt00f
014400081222      *
014500081121      *
014600081110     c                   endIf
014700081110     c                   endIf
014800081210      *
014900081230     c                   if        eseguito = '1'
015000081230     c                   movel(p)  'Errore'      errore
015100081210     c                   endIf
015200081230      *
015300081230      *  Aggiunte nuove testate poichè le tariffe di riferimento
015400081230      *   avevano più periodi contigui.
015500081230     c                   if        eseguito = '2'
015600081230     c                   movel(p)  '+ Testate'   errore
015700081230     c                   endIf
015800081210      *
015900081210      *  se sono qui ho letto un record correttamente
016000081210     c   Of              except    testa
016100081210     c                   except    detta
016200081110      *
016300081110     C                   ENDDO
016400081110     C/EXEC SQL
016500081110     C+ Close A1
016600081110     C/END-EXEC
016700081110     C                   if        Esito < 0
016800081110     c                   return
016900081110     c                   endIF
017000081110     C*
017100081110     C     FINE          TAG
017200081110     c                   except    finest
017300081110     c                   return
017400081209     C*------------------------------------------------------------------------*
017500081209     C* *INZSR - OPERAZIONI INIZIALI
017600081209     C*------------------------------------------------------------------------*
017700081209     C     *INZSR        BEGSR
017800081209      *
017900081209     C     *ENTRY        PLIST
018000081209     C                   PARM                    AGGIORNA          1
018100081209     C*
018200081209     C     fgt01K        kLIST
018300081211     C                   kfld                    stpPDR
018400081211     C                   kfld                    stpSML
018500081211     C                   kfld                    stpTSR
018600081211     C                   kfld                    stpCTR
018700081211     C                   kfld                    stpPRG
018800081209     C*
018900081209     c                   clear                   esito
019000081209      *
019100081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
019200081209     C                   clear                   UDATE_8           8 0
019300081209     C                   MOVEL     *DATE         UDATE_8
019400081209     C*
019500081209     c                   endSR
019600081110     C*---------------------------------------------------------------*
019700090102     c     ANNULLA       begSR
019800081211     C*
019900081211     c                   open      fifpt01l
020000081211     C     fgt01K        setll     fifpt01l
020100081211     C     fgt01K        reade     fifpt01l
020200081211     c                   dow       not %EoF(fifpt01l)
020300081211     c                   delete    fifpt000
020400081211     C     fgt01K        reade     fifpt01l
020500081211     c                   enddo
020600081211     c                   close     fifpt01l
020700081211     C*
020800081211     c                   open      fifpd01l
020900081211     C     fgt01K        setll     fifpd01l
021000081211     C     fgt01K        reade     fifpd01l
021100081211     c                   dow       not %EoF(fifpd01l)
021200081211     c                   delete    fifpd000
021300081211     C     fgt01K        reade     fifpd01l
021400081211     c                   enddo
021500081211     c                   close     fifpd01l
021600081211     C*
021700081211     c                   endSR
021800081211     C*---------------------------------------------------------------*
021900081110     OQSYSPRT   E            TESTA            02
022000081110     O                                           16 'Simul'
022100081110     O                                           25 'Autista'
022200081110     O                                           30 'TSR'
022300081110     O                                           38 'Tariffa'
022400081110     O                                           45 'Progr'
022500081110     O                                           55 'Data_Dec'
022600081110     O                                           65 'Data_Sca'
022700081110     O                                           75 'Data_Stp'
022800081110     O                                           85 'Data_Cnv'
022900081110     O                                           95 'Aut_Rif'
023000081110     O                                          105 'Tar_Rif'
023100081211     O                                          115 'N_Rec_FGT'
023200081110     O          E            DETta       1
023300081110     O                       stpSML              14
023400081110     O                       stpPDR        z     25
023500081110     O                       stpTSR              29
023600081110     O                       stpCTR              36
023700081110     O                       stpPRG        z     43
023800081110     O                       stpDDT        z     55
023900081110     O                       stpDST        z     65
024000081110     O                       stpDTS        z     75
024100081110     O                       stpDCN        z     85
024200081110     O                       stpRAP        z     95
024300081110     O                       stpRCT        z    103
024400081211     o                       FGT_REC       z    115
024500081230     O                       errore             130
024600081110      ****
024700081110     o          E            FINEST      2
024800090102     o                                           30 '*** TNVR15AR ***'
024900090102     o                                           59 '***   FINE  STAMPA  ***'
025000090102     o                                          + 9 '***  ANNULLA TARIFFE ***'
