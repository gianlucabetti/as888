000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200081121     H* TNVR11AR *---------------------------------------------------*
000300081121     H*        Duplica le Tariffe                                    *
000400000000     H*--------------------------------------------------------------*
000500081222     Ffifgt01L  iF   E           K DISK
000600081222     F                                     INFDS(FGTDS)
000700081210     Ffifpt01L  iF   E           K DISK
000800081210     Ffifpt00F  o  a E           K DISK    RENAME(FIFPT000:FIFPTout)
000900081210     F                                     prefix(tes:3)
001000081210     Ffifpd01L  iF   E           K DISK
001100081210     Ffifpd00F  o  a E           K DISK    RENAME(FIFPD000:FIFPDout)
001200081210     F                                     prefix(det:3)
001300081222     D FGTDS           DS
001400081222     D  NrecFGTFis           397    400B 0
001500081110      *
001600081121     D FGT_DS        e ds                  EXTNAME(FIFGT00F)
001700081110      *
001800931202     D KPJBA         E DS
001900081124      *
002000081209     D Data_Oggi       s              8s 0
002100081209      *
002200081209     D prmPDR          s                   like(FgtPDR)
002300081124     D prmSML          s                   like(FgtSML)
002400081124     D prmTSR          s                   like(FgtTSR)
002500081124     D prmCTR          s                   like(FgtCTR)
002600081124     D prmPRG          s                   like(FgtPRG)
002700081124     D prmRAP          s                   like(FgtRAP)
002800081124     D prmRCT          s                   like(FgtRCT)
002900081210     D prmDDT          s                   like(FgtDDT)
003000081210     D prmDST          s                   like(FgtDST)
003100081211     D prmNREC         s              9s 0
003200081229      *
003300081229     D newPRGtes       s                   like(FgtPRG)
003400081229     D dataDDT         s                   like(FgtDDT)
003500081229     D dataDST         s                   like(FgtDST)
003600900521     C*****************************************************************
003700081209      *   Crea le tariffe direttamente sull'autista fotocopiando
003800081209      *    la tariffa di riferimento.
003900081209     C*****************************************************************
004000000000     C     *ENTRY        PLIST
004100081124     C                   PARM                    prmPDR
004200081124     C                   PARM                    prmSML
004300081124     C                   PARM                    prmTSR
004400081124     C                   PARM                    prmCTR
004500081124     C                   PARM                    prmPRG
004600081124     C                   PARM                    prmRAP
004700081124     C                   PARM                    prmRCT
004800081210     c                   parm                    prmDDT
004900081210     c                   parm                    prmDST
005000081211     C                   PARM                    prmNREC
005100081121     C                   PARM                    esito             1
005200081222     C                   PARM                    verifica          1
005300081124      *
005400081124      * Aggancia la tariffa di riferimento
005500081209     C     prm_KEY       kLIST
005600081124     C                   KFLD                    prmRAP
005700081124     C                   KFLD                    prmSML
005800081124     C                   KFLD                    prmTSR
005900081124     C                   KFLD                    prmRCT
006000081124      *
006100081209     C     fgt_KEY       kLIST
006200081209     C                   KFLD                    fgtPDR
006300081209     C                   KFLD                    fgtSML
006400081209     C                   KFLD                    fgtTSR
006500081209     C                   KFLD                    fgtCTR
006600081209     C                   KFLD                    fgtPRG
006700081209      *
006800081110     C                   SETON                                        LR
006900081218      * se non ha trovato il progressivo necessario x la tariffa di riferimento
007000081218      *  deve segnalare l'errore.
007100081218     C                   move      '1'           esito
007200081209     c                   move      *date         Data_Oggi
007300081121      *
007400081209     c                   clear                   trovato           1
007500081209      *
007600081210      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
007700081210      *    poichè non essendo gestito il progressivo sulle tariffe di
007800081210      *     riferimento occorre testare le date di decorrenza - scadenza
007900081210      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
008000081210      *
008100081209      *  Testata Dettagli
008200081209     c     prm_KEY       setll     fiFGT01l
008300081209     c     prm_KEY       reade     fiFGT01l
008400081209      *
008500081209     c                   dow       not %EoF(fiFGT01l)
008600081209      *
008700081210      * Tariffa di riferimento del periodo giusto
008800081210     c                   if        FGTatb = *blank
008900081210     c                   if        prmDDT >= FGTddt and prmDDT <= FGTdst or
009000081210     c                             prmDST >= FGTddt and prmDST <= FGTdst
009100081218     c                   reset                   esito
009200081222     c                   reset                   nuova_testata     1
009300081218      * >>>>>
009400081218     c                   exsr      testata_FPT
009500081218      * >>>>>
009600081218     c                   exsr      dettaglio_FPD
009700081218      * >>>>>
009800081222     c                   end
009900081218     c                   endif
010000081218      *
010100081218     c     prm_KEY       reade     fiFGT01l
010200081218     c                   enddo
010300081218      *
010400081210     C*
010500081110     C     FINE          TAG
010600081110     c                   return
010700081110     C*---------------------------------------------------------------*
010800081218      *   Copia la testata
010900081218     C*---------------------------------------------------------------*
011000081218     c     testata_FPT   begsr
011100081229      *
011200081229      *  comunque una pulizia fuori ciclo
011300081229     c                   clear                   newPRGtes
011400081218      *
011500081218      *  Testata tariffe
011600081218     c     FgT_KEY       setll     fiFPT01l
011700081218     c     FgT_KEY       reade     fiFPT01l
011800081218      *
011900081218     c                   dow       not %EoF(fiFPT01l)
012000081218      *  se non annullato
012100081218     c                   if        fptATB = *blank
012200081229      *
012300081229      *  e una pulizia prima della scrittura del progressivo
012400081229      *   x verificarne l'esistenza.
012500081229     c                   clear                   newPRGtes
012600081218      *
012700081218      *  Campi chiave dell'autista specifico
012800081218     c                   clear                   fiFPTout
012900081218     c                   z-add     prmPDR        tesPDR
013000081218     c                   z-add     prmCTR        tesCTR
013100081218     c                   z-add     prmPRG        tesPRG
013200081218     c                   move      prmSML        tesSML
013300081218     c                   move      prmTSR        tesTSR
013400081218      *
013500081218     c                   move      fptCTD        tesCTD
013600081218     c                   move      fptTAT        tesTAT
013700081218     c                   move      fptTPG        tesTPG
013800081218     c                   move      fptTSM        tesTSM
013900081218     c                   move      fptTPP        tesTPP
014000081218     c                   z-add     fptATA        tesATA
014100081218     c                   z-add     fptARL        tesARL
014200081218     c                   z-add     fptARF        tesARF
014300081218     c                   z-add     fptARO        tesARO
014400081218     c                   z-add     fptRPV        tesRPV
014500081218     c                   move      fptFLR        tesFLR
014600081218     c                   move      fptFTR        tesFTR
014700081218     c                   z-add     fptDTR        tesDTR
014800081218      *
014900081218     c     riscrive_T    tag
015000081218     c                   write     fiFPTout                             99
015100081218      *
015200081218      * se in errore x record duplicato deve incrementare il progressivo
015300081218     c                   if        *in99
015400081229      *
015500081229      *   in questo momento lo imposta e lo incrementa quindi lo reimposta sul file
015600081229     c                   if        newPRGtes = 0
015700081229     c                   z-add     tesPRG        newPRGtes
015800081229     c                   end
015900081229      *
016000081229     c                   add       1             newPRGtes
016100081229     c                   z-add     newPRGtes     tesPRG
016200081229      *
016300081222     C                   move      'S'           verifica
016400081222     C                   move      'S'           nuova_testata
016500081218     c                   goto      riscrive_T
016600081229      *
016700081218     c                   end
016800081229      **
016900081229      *   se si è scritto un nuovo progressivo occorre scrivere la relativa testata
017000081229     C                   If        nuova_testata <> *blank
017100081229      **
017200081229      ** ----------------------------------------------------------------------
017300081229      **  Occorre demandare esternamente la scrittura della testata su un altro file
017400081229      **   di work identico al FIFGT per non andare ad intralciare il ciclo di
017500081229      **   lettura delle testate di riferimento in questo programma.....poi,
017600081229      **   alla fine di tutto, il file di work verrà copiato sul FIFGT in ADD.
017700081229      **
017800081229      ** esempio: tar.  C/300 riferita all'AUT xxx
017900081229      **   tempo:
018000081229      **     Date sulla linea temporale della tariffa e di quella di riferimento
018100081229      **     >>>--------1----2----------3--4---------------5---------->>>>
018200081229      **
018300081229      **   tariffa AUT       DDT            prm                  DST
018400081229      **    da PARM          |---------------------------------- oo
018500081229      **                    (2)
018600081229      **                DDT           DST DDT             DST
018700081229      **  Tariffa di    (1)    fgt 1°  (3)(4)   fgt 2°    (5)
018800081229      **           RIFER |---------------|-----------------|
018900081229      **
019000081229      ** Le 2 testate avranno i seguenti periodi per l'autista
019100081229      **  1° periodo :  DDT = (2)    e   DST = (3)
019200081229      **  2° periodo :  DDT = (4)    e   DST = (5)
019300081229      **
019400081229      ** ----------------------------------------------------------------------
019500081229      ** Imposta i limiti della nuova testata da scrivere
019600081229      **
019700081229     c                   if        prmDDT >= fgtDDT
019800081229     c                   eval        dataDDT = prmDDT
019900081229     c                   else
020000081229     c                   eval        dataDDT = fgtDDT
020100081229     c                   end
020200081229      **
020300081229     c                   if        prmDST <= fgtDDT
020400081229     c                   eval        dataDST = prmDST
020500081229     c                   else
020600081229     c                   eval        dataDST = fgtDST
020700081229     c                   end
020800081229      **
020900081229     c                   call      'TNVR12AR'
021000081229     c                   parm                    prmNREC
021100081229     c                   parm                    newPRGtes
021200081229     c                   parm                    dataDDT
021300081229     c                   parm                    dataDST
021400081229      ****
021500081229     c****               z-add     *date         fisDUV
021600081229     c****               eval      FisDCV = ' Creata da Tar.Rifer.'
021700081229      ****
021800081229     c                   eval      esito = '1'
021900081229     c                   end
022000081229      **
022100081218     c                   endif
022200081218      *
022300081218     c     FgT_KEY       reade     fiFPT01l
022400081218     c                   enddo
022500081218      *
022600081218     c                   endsr
022700081218     C*---------------------------------------------------------------*
022800081218      *   Copia il dettaglio
022900081218     C*---------------------------------------------------------------*
023000081218     c     dettaglio_FPD begsr
023100081218      *
023200081218      *  Dettaglio tariffe
023300081218     c     FgT_KEY       setll     fiFPD01l
023400081218     c     FgT_KEY       reade     fiFPD01l
023500081218      *
023600081218     c                   dow       not %EoF(fiFPD01l)
023700081218      *  se non annullato
023800081218     c                   if        fpdATB = *blank
023900081218      *
024000081218      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
024100081218      *    poichè non essendo gestito il progressivo sulle tariffe di
024200081218      *     riferimento occorre testare le date di decorrenza - scadenza
024300081218      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
024400081218      *
024500081218      *  Campi chiave dell'autista specifico
024600081218     c                   clear                   fiFPDout
024700081218     c                   z-add     prmPDR        detPDR
024800081218     c                   z-add     prmCTR        detCTR
024900081218     c                   z-add     prmPRG        detPRG
025000081218     c                   move      prmSML        detSML
025100081218     c                   move      prmTSR        detTSR
025200081218      *
025300081218     c                   move      fpdCTD        detCTD
025400081218     c                   move      fpdCAP        detCAP
025500081218     c                   z-add     fpdSGL        detSGL
025600081218     c                   z-add     fpdITR        detITR
025700081218     c                   z-add     fpdMIN        detMIN
025800081218     c                   z-add     fpdMAX        detMAX
025900081218     c                   move      fpdAIN        detAIN
026000081218     c                   move      fpdFTR        detFTR
026100081218     c                   z-add     fpdDTR        detDTR
026200081218      *
026300081218     c                   write     fiFPDout
026400081218      *
026500081218     c                   endif
026600081218      *
026700081218     c     FgT_KEY       reade     fiFPD01l
026800081218     c                   enddo
026900081218      *
027000081218     c                   endsr
027100081218     C*---------------------------------------------------------------*
