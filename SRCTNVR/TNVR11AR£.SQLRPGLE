000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200081121     H* TNVR11AR *---------------------------------------------------*
000300081121     H*        Duplica le Tariffe                                    *
000400000000     H*--------------------------------------------------------------*
000500081222     Ffifgt01L  iF   E           K DISK
000600081222     F                                     INFDS(FGTDS)
000700081210     Ffifpt01L  iF   E           K DISK
000800081230     Ffifpt00F  o  a E           K DISK    RENAME(FIFPT000:FIFPTout)
000900081210     F                                     prefix(tes:3)
001000081210     Ffifpd01L  iF   E           K DISK
001100081230     Ffifpd00F  o  a E           K DISK    RENAME(FIFPD000:FIFPDout)
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
003400081230     D savPRG          s                   like(FgtPRG)
003500081229     D dataDDT         s                   like(FgtDDT)
003600081229     D dataDST         s                   like(FgtDST)
003700081230     D sv_FPTPDR       s                   like(FptPDR)
003800081230     D sv_FPTSML       s                   like(FptSML)
003900081230     D sv_FPTTSR       s                   like(FptTSR)
004000081230     D sv_FPTCTR       s                   like(FptCTR)
004100081230     D sv_FPTPRG       s                   like(FptPRG)
004200081230      *
004300081230     D returnDDT       s                   like(FgtDDT)
004400081230     D returnDST       s                   like(FgtDST)
004500900521     C*****************************************************************
004600081209      *   Crea le tariffe direttamente sull'autista fotocopiando
004700081209      *    la tariffa di riferimento.
004800081209     C*****************************************************************
004900000000     C     *ENTRY        PLIST
005000081124     C                   PARM                    prmPDR
005100081124     C                   PARM                    prmSML
005200081124     C                   PARM                    prmTSR
005300081124     C                   PARM                    prmCTR
005400081124     C                   PARM                    prmPRG
005500081124     C                   PARM                    prmRAP
005600081124     C                   PARM                    prmRCT
005700081210     c                   parm                    prmDDT
005800081210     c                   parm                    prmDST
005900081230     c                   parm                    returnDST
006000081211     C                   PARM                    prmNREC
006100081121     C                   PARM                    esito             1
006200081222     C                   PARM                    verifica          1
006300081124      *
006400081124      * Aggancia la tariffa di riferimento
006500081230     C     parm_KEY      kLIST
006600081124     C                   KFLD                    prmRAP
006700081124     C                   KFLD                    prmSML
006800081124     C                   KFLD                    prmTSR
006900081124     C                   KFLD                    prmRCT
007000081124      *
007100081209     C     fgt_KEY       kLIST
007200081209     C                   KFLD                    fgtPDR
007300081209     C                   KFLD                    fgtSML
007400081209     C                   KFLD                    fgtTSR
007500081209     C                   KFLD                    fgtCTR
007600081209     C                   KFLD                    fgtPRG
007700081209      *
007800081110     C                   SETON                                        LR
007900081218      * se non ha trovato il progressivo necessario x la tariffa di riferimento
008000081218      *  deve segnalare l'errore.
008100081218     C                   move      '1'           esito
008200081209     c                   move      *date         Data_Oggi
008300081230     C                   clear                   prima_volta       1
008400081121      *
008500081209     c                   clear                   trovato           1
008600081209      *
008700081210      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
008800081210      *    poichè non essendo gestito il progressivo sulle tariffe di
008900081210      *     riferimento occorre testare le date di decorrenza - scadenza
009000081210      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
009100081210      *
009200081209      *  Testata Dettagli
009300081230     c     parm_KEY      setll     fiFGT01l
009400081230     c     parm_KEY      reade     fiFGT01l
009500081209      *
009600081209     c                   dow       not %EoF(fiFGT01l)
009700081230      *
009800081210     c                   if        FGTatb = *blank
009900081230      *
010000081230      * Tariffa di riferimento del periodo giusto
010100081210     c                   if        prmDDT >= FGTddt and prmDDT <= FGTdst or
010200081210     c                             prmDST >= FGTddt and prmDST <= FGTdst
010300081230      **
010400081230      **  Data del periodo riferito da riportare sul record di testata aggiornato
010500081230      * >>>>>
010600081230     C                   if          prima_volta = *blank
010700081230     C                   eval      prima_volta = 'S'
010800081230      **
010900081230     c                   if        prmDST >  fgtDST
011000081230     c                   eval      returnDST = fgtDST
011100081230     c                   end
011200081230      **
011300081230     c                   end
011400081230      * >>>>>
011500081230      **
011600081218     c                   reset                   esito
011700081222     c                   reset                   nuova_testata     1
011800081218      * >>>>>
011900081218     c                   exsr      testata_FPT
012000081218      * >>>>>
012100081218     c                   exsr      dettaglio_FPD
012200081218      * >>>>>
012300081222     c                   end
012400081230      *
012500081218     c                   endif
012600081218      *
012700081230     c     parm_KEY      reade     fiFGT01l
012800081218     c                   enddo
012900081218      *
013000081210     C*
013100081110     C     FINE          TAG
013200081110     c                   return
013300081110     C*---------------------------------------------------------------*
013400081218      *   Copia la testata
013500081218     C*---------------------------------------------------------------*
013600081218     c     testata_FPT   begsr
013700081229      *
013800081229      *  comunque una pulizia fuori ciclo
013900081230     c                   clear                   savPRG
014000081218      *
014100081218      *  Testata tariffe
014200081218     c     FgT_KEY       setll     fiFPT01l
014300081218     c     FgT_KEY       reade     fiFPT01l
014400081218      *
014500081218     c                   dow       not %EoF(fiFPT01l)
014600081230      *  se non annullato
014700081230     c                   if        fptATB = *blank
014800081229      *
014900081229      *  e una pulizia prima della scrittura del progressivo
015000081229      *   x verificarne l'esistenza.
015100081229     c                   clear                   newPRGtes
015200081218      *
015300081218      *  Campi chiave dell'autista specifico
015400081218     c                   clear                   fiFPTout
015500081218     c                   z-add     prmPDR        tesPDR
015600081218     c                   z-add     prmCTR        tesCTR
015700081218     c                   z-add     prmPRG        tesPRG
015800081218     c                   move      prmSML        tesSML
015900081218     c                   move      prmTSR        tesTSR
016000081218      *
016100081218     c                   move      fptCTD        tesCTD
016200081218     c                   move      fptTAT        tesTAT
016300081218     c                   move      fptTPG        tesTPG
016400081218     c                   move      fptTSM        tesTSM
016500081218     c                   move      fptTPP        tesTPP
016600081218     c                   z-add     fptATA        tesATA
016700081218     c                   z-add     fptARL        tesARL
016800081218     c                   z-add     fptARF        tesARF
016900081218     c                   z-add     fptARO        tesARO
017000081218     c                   z-add     fptRPV        tesRPV
017100081218     c                   move      fptFLR        tesFLR
017200081218     c                   move      fptFTR        tesFTR
017300081218     c                   z-add     fptDTR        tesDTR
017400081218      *
017500081218     c     riscrive_T    tag
017600081218     c                   write     fiFPTout                             99
017700081218      *
017800081218      * se in errore x record duplicato deve incrementare il progressivo
017900081218     c                   if        *in99
018000081229      *
018100081229      *   in questo momento lo imposta e lo incrementa quindi lo reimposta sul file
018200081229     c                   if        newPRGtes = 0
018300081229     c                   z-add     tesPRG        newPRGtes
018400081229     c                   end
018500081229      *
018600081229     c                   add       1             newPRGtes
018700081229     c                   z-add     newPRGtes     tesPRG
018800081229      *
018900081222     C                   move      'S'           verifica
019000081222     C                   move      'S'           nuova_testata
019100081218     c                   goto      riscrive_T
019200081229      *
019300081218     c                   end
019400081229      **
019500081229      *   se si è scritto un nuovo progressivo occorre scrivere la relativa testata
019600081230     C                   If        nuova_testata <> *blank  and
019700081230     c                             newPRGtes <> savPRG
019800081230     c                   z-add     newPRGtes     savPRG
019900081229      **
020000081229      ** ----------------------------------------------------------------------
020100081229      **  Occorre demandare esternamente la scrittura della testata su un altro file
020200081229      **   di work identico al FIFGT per non andare ad intralciare il ciclo di
020300081229      **   lettura delle testate di riferimento in questo programma.....poi,
020400081229      **   alla fine di tutto, il file di work verrà copiato sul FIFGT in ADD.
020500081229      **
020600081229      ** esempio: tar.  C/300 riferita all'AUT xxx
020700081229      **   tempo:
020800081229      **     Date sulla linea temporale della tariffa e di quella di riferimento
020900081229      **     >>>--------1----2----------3--4---------------5---------->>>>
021000081229      **
021100081229      **   tariffa AUT       DDT            prm                  DST
021200081229      **    da PARM          |---------------------------------- oo
021300081229      **                    (2)
021400081229      **                DDT           DST DDT             DST
021500081229      **  Tariffa di    (1)    fgt 1°  (3)(4)   fgt 2°    (5)
021600081229      **           RIFER |---------------|-----------------|
021700081229      **
021800081229      ** Le 2 testate avranno i seguenti periodi per l'autista
021900081229      **  1° periodo :  DDT = (2)    e   DST = (3)
022000081229      **  2° periodo :  DDT = (4)    e   DST = (5)
022100081229      **
022200081229      ** ----------------------------------------------------------------------
022300081229      ** Imposta i limiti della nuova testata da scrivere
022400081229      **
022500081229     c                   if        prmDDT >= fgtDDT
022600081229     c                   eval        dataDDT = prmDDT
022700081229     c                   else
022800081229     c                   eval        dataDDT = fgtDDT
022900081229     c                   end
023000081229      **
023100081230     c                   if        prmDST <= fgtDST
023200081229     c                   eval        dataDST = prmDST
023300081229     c                   else
023400081229     c                   eval        dataDST = fgtDST
023500081229     c                   end
023600081229      **
023700081229     c                   call      'TNVR12AR'
023800081229     c                   parm                    prmNREC
023900081229     c                   parm                    newPRGtes
024000081229     c                   parm                    dataDDT
024100081229     c                   parm                    dataDST
024200081229      **
024300081230      *  Ha scritto delle nuove testate
024400081230     C                   move      '2'           esito
024500081229     c                   end
024600081229      **
024700081218     c                   endif
024800081218      *
024900081218     c     FgT_KEY       reade     fiFPT01l
025000081218     c                   enddo
025100081230      *
025200081218     c                   endsr
025300081218     C*---------------------------------------------------------------*
025400081218      *   Copia il dettaglio
025500081218     C*---------------------------------------------------------------*
025600081218     c     dettaglio_FPD begsr
025700081218      *
025800081218      *  Dettaglio tariffe
025900081218     c     FgT_KEY       setll     fiFPD01l
026000081218     c     FgT_KEY       reade     fiFPD01l
026100081218      *
026200081218     c                   dow       not %EoF(fiFPD01l)
026300081218      *  se non annullato
026400081218     c                   if        fpdATB = *blank
026500081218      *
026600081218      *   Bisogna prendere la tariffa di riferimento in vigore nel periodo
026700081218      *    poichè non essendo gestito il progressivo sulle tariffe di
026800081218      *     riferimento occorre testare le date di decorrenza - scadenza
026900081218      *   CHE PERO' STANNO SULLA TESTATA DEL FIFGT di RIFERIMENTO
027000081218      *
027100081218      *  Campi chiave dell'autista specifico
027200081218     c                   clear                   fiFPDout
027300081218     c                   z-add     prmPDR        detPDR
027400081218     c                   z-add     prmCTR        detCTR
027500081230      *
027600081230     c                   z-add     tesPRG        detPRG
027700081230      *
027800081218     c                   move      prmSML        detSML
027900081218     c                   move      prmTSR        detTSR
028000081218      *
028100081218     c                   move      fpdCTD        detCTD
028200081218     c                   move      fpdCAP        detCAP
028300081218     c                   z-add     fpdSGL        detSGL
028400081218     c                   z-add     fpdITR        detITR
028500081218     c                   z-add     fpdMIN        detMIN
028600081218     c                   z-add     fpdMAX        detMAX
028700081218     c                   move      fpdAIN        detAIN
028800081218     c                   move      fpdFTR        detFTR
028900081218     c                   z-add     fpdDTR        detDTR
029000081218      *
029100081230     c                   write     fiFPDout
029200081218      *
029300081218     c                   endif
029400081218      *
029500081218     c     FgT_KEY       reade     fiFPD01l
029600081218     c                   enddo
029700081218      *
029800081218     c                   endsr
029900081218     C*---------------------------------------------------------------*
