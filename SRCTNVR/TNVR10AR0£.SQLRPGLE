000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200081110     H* TNVR1AR *----------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800081218     Ffifgt01L  iF   E           K DISK
000900081218     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
001000081222     F                                     usropn
001100081216     Fwffgt00f  o  a E           K DISK    prefix(WF:2)
001200081211      *
001300081211     Ffifpt01L  uF   E           K DISK    usropn
001400081211     Ffifpd01L  uF   E           K DISK    usropn
001500081211      *
001600081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700081211      * -------------------------------------------------------------
001800081110     D dsFGT         E DS                  ExtName(FIFGT00F)
001900081110     d                                     prefix(STP:3)
002000081110     D elimina_riferimento...
002100081110     D                 s              2a
002200081110     D esito           s             10i 0
002300081110     D FGT_REC         s              9s 0
002400931202     D KPJBA         E DS
002500900521     C*****************************************************************
002600081209     C                   SETON                                        LR
002700020418     C*
002800081110     C/EXEC SQL DECLARE A1 CURSOR FOR
002900081110     C+ SELECT rrn(fifgt00f), fifgt00f.* FROM
003000081210     C+ fifgt00f WHERE FGTTSR in ('R', 'C', 'G') and FGTRAP >0
003100081209     C+ ORDER BY
003200081110     C+ FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
003300081110     C/END-EXEC
003400081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
003500081110     c                   if        sqlCODE < 0
003600081110     c                   eval      esito = Sqlcode
003700081110     c                   dump(a)
003800081110     c                   return
003900081110     c                   end
004000081110     C/EXEC SQL
004100081110     C+ OPEN A1
004200081110     C/END-EXEC
004300081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
004400081110     c                   if        sqlCODE < 0
004500081110     c                   eval      esito = Sqlcode
004600081110     c                   dump(a)
004700081110     c                   return
004800081110     c                   else
004900081110     c                   except    testa
005000081110     c                   end
005100081110      *
005200081209      * resto nel ciclo finchè....
005300081110      *
005400081110     C                   DOU       SqlCod < 0
005500081110      *
005600081110      * lettura cursore
005700081110     C/EXEC SQL
005800081110     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
005900081110     C/END-EXEC
006000081110      *  EoF
006100081110     C                   if        SqlCod = 100
006200081110     c                   Leave
006300081110      *  x errori
006400081110     C                   ElseIf    SqlCod < 0
006500081110     c                   eval      esito = Sqlcode
006600081110     c                   dump(a)
006700081110     c                   leave
006800081110     C                   ENDif
006900081110      *
007000081210     c                   clear                   eseguito
007100081222     c                   clear                   verifica
007200081210     c                   clear                   errore            6
007300081110      *
007400081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
007500081110     c                   if        Aggiorna = 'S'
007600081110      *
007700081216      *  imposta il file di work a pro-memoria delle tariffe di riferimento
007800081216      *   da eliminare in seguito
007900081216     c                   clear                   wffgt000
008000081217     c                   z-add     STPPDR        wfTPDR
008100081217     c                   move      STPSML        wfTSML
008200081217     c                   move      STPTSR        wfTTSR
008300081217     c                   move      STPCTR        wfTCTR
008400081217     c                   z-add     STPPRG        wfTPRG
008500081217     c                   z-add     STPRAP        wfTRAP
008600081217     c                   move      STPRCT        wfTRCT
008700081216      *
008800081211      * Se erano presenti records su FPT e FPD anche annullati li elimina
008900081211      *  prima di scrivere con il programma seguente
009000081211     C                   EXSR      PULISCE
009100081211      *
009200081110     C     fgt01K        chain     fifgt01l
009300081211     c                   if        %Found(fifgt01l)
009400081121      *
009500081121      * duplica la tariffa da tariffa di riferimento
009600081209     c                   call      'TNVR11AR'
009700081121     c                   parm                    FGTPDR
009800081121     c                   parm                    FGTSML
009900081121     c                   parm                    FGTTSR
010000081121     c                   parm                    FGTCTR
010100081121     c                   parm                    FGTPRG
010200081121     c                   parm                    fgtRAP
010300081121     c                   parm                    fgtRCT
010400081210     c                   parm                    fgtDDT
010500081210     c                   parm                    fgtDST
010600081209     c                   parm                    fgt_REC
010700081209     c                   parm                    eseguito          1
010800081222     c                   parm                    verifica          1
010900081222      *
011000081222     c                   if        verifica <> *blank
011100081222     c                   eval      WFtNOTE = 'Da Verificare'
011200081222     c                   end
011300081222      *
011400081222     c                   write     wffgt000
011500081121      *
011600081121      *  Solo se non ci sono stati errori:
011700081218      *   deve eliminare i puntamenti del codice di riferimento
011800081209     c                   if        eseguito = *blank
011900081222      *
012000081222     c                   open      fifgt00f
012100081218     c     fgt_REC       chain     FIFGT00F
012200081218     c                   if        %Found(FIFGT00F)
012300081110     c                   clear                   fgtrap
012400081110     c                   clear                   fgtrct
012500081218     c                   update    fgtFisico
012600081121     c                   endif
012700081222     c                   close     fifgt00f
012800081222      *
012900081218     c                   endif
013000081121      *
013100081110     c                   endIf
013200081110     c                   endIf
013300081210      *
013400081210     c                   if        eseguito <>*blank
013500081210     c                   movel     'Errore'      errore
013600081210     c                   endIf
013700081210      *
013800081210      *  se sono qui ho letto un record correttamente
013900081210     c   Of              except    testa
014000081210     c                   except    detta
014100081110      *
014200081110     C                   ENDDO
014300081110     C/EXEC SQL
014400081110     C+ Close A1
014500081110     C/END-EXEC
014600081110     C                   if        Esito < 0
014700081110     c                   return
014800081110     c                   endIF
014900081110     C*
015000081110     C     FINE          TAG
015100081110     c                   except    finest
015200081110     c                   return
015300081209     C*------------------------------------------------------------------------*
015400081209     C* *INZSR - OPERAZIONI INIZIALI
015500081209     C*------------------------------------------------------------------------*
015600081209     C     *INZSR        BEGSR
015700081209      *
015800081209     C     *ENTRY        PLIST
015900081209     C                   PARM                    AGGIORNA          1
016000081209     C*
016100081209     C     fgt01K        kLIST
016200081211     C                   kfld                    stpPDR
016300081211     C                   kfld                    stpSML
016400081211     C                   kfld                    stpTSR
016500081211     C                   kfld                    stpCTR
016600081211     C                   kfld                    stpPRG
016700081209     C*
016800081209     c                   clear                   esito
016900081209      *
017000081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
017100081209     C                   clear                   UDATE_8           8 0
017200081209     C                   MOVEL     *DATE         UDATE_8
017300081209     C*
017400081209     c                   endSR
017500081110     C*---------------------------------------------------------------*
017600081211     c     PULISCE       begSR
017700081211     C*
017800081211     c                   open      fifpt01l
017900081211     C     fgt01K        setll     fifpt01l
018000081211     C     fgt01K        reade     fifpt01l
018100081211     c                   dow       not %EoF(fifpt01l)
018200081211     c                   delete    fifpt000
018300081211     C     fgt01K        reade     fifpt01l
018400081211     c                   enddo
018500081211     c                   close     fifpt01l
018600081211     C*
018700081211     c                   open      fifpd01l
018800081211     C     fgt01K        setll     fifpd01l
018900081211     C     fgt01K        reade     fifpd01l
019000081211     c                   dow       not %EoF(fifpd01l)
019100081211     c                   delete    fifpd000
019200081211     C     fgt01K        reade     fifpd01l
019300081211     c                   enddo
019400081211     c                   close     fifpd01l
019500081211     C*
019600081211     c                   endSR
019700081211     C*---------------------------------------------------------------*
019800081110     OQSYSPRT   E            TESTA            02
019900081110     O                                           16 'Simul'
020000081110     O                                           25 'Autista'
020100081110     O                                           30 'TSR'
020200081110     O                                           38 'Tariffa'
020300081110     O                                           45 'Progr'
020400081110     O                                           55 'Data_Dec'
020500081110     O                                           65 'Data_Sca'
020600081110     O                                           75 'Data_Stp'
020700081110     O                                           85 'Data_Cnv'
020800081110     O                                           95 'Aut_Rif'
020900081110     O                                          105 'Tar_Rif'
021000081211     O                                          115 'N_Rec_FGT'
021100081211     O                                          122 'Errori'
021200081110     O          E            DETta       1
021300081110     O                       stpSML              14
021400081110     O                       stpPDR        z     25
021500081110     O                       stpTSR              29
021600081110     O                       stpCTR              36
021700081110     O                       stpPRG        z     43
021800081110     O                       stpDDT        z     55
021900081110     O                       stpDST        z     65
022000081110     O                       stpDTS        z     75
022100081110     O                       stpDCN        z     85
022200081110     O                       stpRAP        z     95
022300081110     O                       stpRCT        z    103
022400081211     o                       FGT_REC       z    115
022500081230     O                       errore             122
022600081110      ****
022700081110     o          E            FINEST      2
022800081110     o                                           59 '***   FINE  STAMPA  ***'
