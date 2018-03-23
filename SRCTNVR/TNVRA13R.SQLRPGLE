000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090112     H* TNVRa13R *---------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800081218     Ffifgt00f  uF   E             DISK    rename(FIFGT000:FGTFisico)
000900081222     F                                     usropn
001000081211      *
001100081211     Ffifpt01L  uF   E           K DISK    usropn
001200081211     Ffifpd01L  uF   E           K DISK    usropn
001300081211      *
001400081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001500081211      * -------------------------------------------------------------
001600081110     D dsFGT         E DS                  ExtName(FIFGT00F)
001700081110     d                                     prefix(STP:3)
001800081110     D esito           s             10i 0
001900081110     D FGT_REC         s              9s 0
002000931202     D KPJBA         E DS
002100081230      *
002200900521     C*****************************************************************
002300081209     C                   SETON                                        LR
002400020418     C*
002500081110     C/EXEC SQL DECLARE A1 CURSOR FOR
002600081110     C+ SELECT rrn(fifgt00f), fifgt00f.* FROM
002700090112     C+ fifgt00f WHERE FGTTSR in ('R', 'C', 'G')
002800090112     C+ and fgtATB = ' ' and FGTddt > 20091231
002900090102     C+ ORDER BY
003000081110     C+ FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
003100081110     C/END-EXEC
003200081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
003300081110     c                   if        sqlCODE < 0
003400081110     c                   eval      esito = Sqlcode
003500081110     c                   dump(a)
003600081110     c                   return
003700081110     c                   end
003800081110     C/EXEC SQL
003900081110     C+ OPEN A1
004000081110     C/END-EXEC
004100081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
004200081110     c                   if        sqlCODE < 0
004300081110     c                   eval      esito = Sqlcode
004400081110     c                   dump(a)
004500081110     c                   return
004600081110     c                   else
004700081110     c                   except    testa
004800081110     c                   end
004900081110      *
005000081209      * resto nel ciclo finchè....
005100081110     C                   DOU       SqlCod < 0
005200081110      * lettura cursore
005300081110     C/EXEC SQL
005400081110     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
005500081110     C/END-EXEC
005600081110      *  EoF
005700081110     C                   if        SqlCod = 100
005800081110     c                   Leave
005900081110      *  x errori
006000081110     C                   ElseIf    SqlCod < 0
006100081110     c                   eval      esito = Sqlcode
006200081110     c                   dump(a)
006300081110     c                   leave
006400081110     C                   ENDif
006500081110      *
006600090102     c                   clear                   errore           10
006700081110      *
006800081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
006900081110     c                   if        Aggiorna = 'S'
007000081216      *
007100081211      * Se erano presenti records su FPT e FPD anche annullati li elimina
007200081211      *  prima di scrivere con il programma seguente
007300081211     C                   EXSR      PULISCE
007400081211      *
007500081121      *  Solo se non ci sono stati errori:
007600081218      *   deve eliminare i puntamenti del codice di riferimento
007700081222     c                   open      fifgt00f
007800081218     c     fgt_REC       chain     FIFGT00F
007900081218     c                   if        %Found(FIFGT00F)
008000090112     c                   delete    fgtFisico
008100081121     c                   endif
008200081222     c                   close     fifgt00f
008300090112      *
008400081110     c                   endIf
008500081210      *
008600081210      *  se sono qui ho letto un record correttamente
008700081210     c   Of              except    testa
008800081210     c                   except    detta
008900081110      *
009000081110     C                   ENDDO
009100081110     C/EXEC SQL
009200081110     C+ Close A1
009300081110     C/END-EXEC
009400081110     C                   if        Esito < 0
009500081110     c                   return
009600081110     c                   endIF
009700081110     C*
009800081110     C     FINE          TAG
009900081110     c                   except    finest
010000081110     c                   return
010100081209     C*------------------------------------------------------------------------*
010200081209     C* *INZSR - OPERAZIONI INIZIALI
010300081209     C*------------------------------------------------------------------------*
010400081209     C     *INZSR        BEGSR
010500081209      *
010600081209     C     *ENTRY        PLIST
010700081209     C                   PARM                    AGGIORNA          1
010800081209     C*
010900081209     C     fgt01K        kLIST
011000081211     C                   kfld                    stpPDR
011100081211     C                   kfld                    stpSML
011200081211     C                   kfld                    stpTSR
011300081211     C                   kfld                    stpCTR
011400081211     C                   kfld                    stpPRG
011500081209     C*
011600081209     c                   clear                   esito
011700081209      *
011800081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
011900081209     C                   clear                   UDATE_8           8 0
012000081209     C                   MOVEL     *DATE         UDATE_8
012100081209     C*
012200081209     c                   endSR
012300081110     C*---------------------------------------------------------------*
012400081211     c     PULISCE       begSR
012500081211     C*
012600081211     c                   open      fifpt01l
012700081211     C     fgt01K        setll     fifpt01l
012800081211     C     fgt01K        reade     fifpt01l
012900081211     c                   dow       not %EoF(fifpt01l)
013000081211     c                   delete    fifpt000
013100081211     C     fgt01K        reade     fifpt01l
013200081211     c                   enddo
013300081211     c                   close     fifpt01l
013400081211     C*
013500081211     c                   open      fifpd01l
013600081211     C     fgt01K        setll     fifpd01l
013700081211     C     fgt01K        reade     fifpd01l
013800081211     c                   dow       not %EoF(fifpd01l)
013900081211     c                   delete    fifpd000
014000081211     C     fgt01K        reade     fifpd01l
014100081211     c                   enddo
014200081211     c                   close     fifpd01l
014300081211     C*
014400081211     c                   endSR
014500081211     C*---------------------------------------------------------------*
014600081110     OQSYSPRT   E            TESTA            02
014700081110     O                                           16 'Simul'
014800081110     O                                           25 'Autista'
014900081110     O                                           30 'TSR'
015000081110     O                                           38 'Tariffa'
015100081110     O                                           45 'Progr'
015200081110     O                                           55 'Data_Dec'
015300081110     O                                           65 'Data_Sca'
015400081110     O                                           75 'Data_Stp'
015500081110     O                                           85 'Data_Cnv'
015600081110     O                                           95 'Aut_Rif'
015700081110     O                                          105 'Tar_Rif'
015800081211     O                                          115 'N_Rec_FGT'
015900081110     O          E            DETta       1
016000081110     O                       stpSML              14
016100081110     O                       stpPDR        z     25
016200081110     O                       stpTSR              29
016300081110     O                       stpCTR              36
016400081110     O                       stpPRG        z     43
016500081110     O                       stpDDT        z     55
016600081110     O                       stpDST        z     65
016700081110     O                       stpDTS        z     75
016800081110     O                       stpDCN        z     85
016900081110     O                       stpRAP        z     95
017000081110     O                       stpRCT        z    103
017100081211     o                       FGT_REC       z    115
017200081230     O                       errore             130
017300081110      ****
017400081110     o          E            FINEST      2
017500090112     o                                           30 '*** TNVRA13R ***'
017600090102     o                                           59 '***   FINE  STAMPA  ***'
017700090112     o                                          + 9 '***  ELIMINA TARIFFE'
017800090112     o                                          + 1 'oltre 2010 ***'
