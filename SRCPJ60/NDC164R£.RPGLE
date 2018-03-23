000100981218     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300970107     H*PARMS CVTOPT(*NONE)
000400950608     H DECEDIT('0,') DATEDIT(*DMY.)
000500000511     F*-------------
000600000511     F*
000700971212     F*                   STAMPA RIEPILOGO LIBRI IVA
000800971212     F*                 ------------------------------
000900000511     F*
001000971212     F*-------------
001100040308     FNDIVA03J  IF   E           K DISK
001200040308     FNDIVA04J  IF   E           K DISK
001300020506C1550 * Soc/TpReg/Libro/Aliq/RifIva/DtAnnot
001400040308     FNDIVA09J  IF   E           K DISK
001500020506C1550 * Soc/TpReg/Aliq/RifIva/DtAnnot
001600040308     FNDIVA10J  IF   E           K DISK
001700040308     FNDMOV01L  IF   E           K DISK
001800040308     FANRIV01L  IF   E           K DISK
001900040308     FNDIVAA198 O    E             PRINTER OFLIND(*IN39)
002000130620     F                                     INFDS(YInfNDIvaA)
002100040308     FNDIVAV198 O    E             PRINTER OFLIND(*IN40)
002200130620     F                                     INFDS(YInfNDIvaV)
002300040308     FNDIVAC198 O    E             PRINTER OFLIND(*IN41)
002400130620
002500130620     D Annullato       S             19    INZ(' A N N U L L A T O ')
002600130620     D*-------------
002700130620     D YInfNDIvaA      DS
002800130620     D  CurLineA             367    368I 0
002900130620     D*-------------
003000130620     D YInfNDIvaV      DS
003100130620     D  CurLineV             367    368I 0
003200950627     D*-------------
003300900731     D KPJBA         E DS
003400950605     D*-------------
003500950130     D* Parametri del PGM
003600950703     D C164DS        E DS                  EXTNAME(NDC164DS)
003700950628     D*-------------
003800941103     D* Dati per pgm XSOC
003900941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
004000000503C1357D    XScIvaVent                  1A   Overlay(XscFgo:4)
004100030115C1611D    XScDare                     1    overlay(XScFgo:7)
004200030115C1611D    XScAvere                    1    overlay(XScFgo:8)
004300020124     D*-------------
004400020124c1529D* Parametri memorizzazione intestazione
004500020124  "  D xpaout          DS          2000
004600020124  "  D* DS esterna per stampa intestazione bollato
004700020124  "  D ndc099ds      E DS
004800020124  "  D*-------------
004900020124  "  D* Tabella numeri pagina
005000020124  "  D Ang008Ds      E DS
005100020124  "  D*-------------
005200020124  "  D* Richiamo a XATB
005300020124c1529D XATBDS        E DS                  INZ
005400030418     D*-------------
005500030418C1651D* Parametro modulo IVA
005600030418  "  D ANP045DS      E DS
005700030418c1651D*-------------
005800950605     D* DS Interna per dati utente
005900941103     D XSOCDS          DS          1000
006000950704     D*-------------
006100950704     D* Reperimento nome PGM
006200950704     D STATUS         SDS           333
006300950704     D  DSPGM            *PROC
006400971212     D*-------------
006500971212     D wwdatactr       S               D
006600971212     D*-------------
006700000118C1309D ZDateISO        S               D
006800000420     D*-------------
006900000511C1357D* Parametri del pgm NDC267R (analisi acquisti ventilazione)
007000000427  "  D NDC267DS      E DS
007100000427  "  D*-------------
007200000427  "  D* Dimensione skiere
007300000427  "  D $DimSk          C                   Const(50)
007400000427  "  D*-------------
007500000511  "  D* Skiere sintesi dati acquisti ventilazione
007600000427  "  D DsDati          DS
007700000427  "  D  SkDati                       46A   DIM($DimSk)
007800000427  "  D  SkRifAlq                      8A   Overlay(SkDati:1)
007900000427  "  D  SkImporto                    19S 3 Overlay(SkDati:9)
008000000427  "  D  SkImponib                    19S 3 Overlay(SkDati:28)
008100000427  "  D*-------------
008200000511  "  D* Ds rif.iva/aliquota
008300000426  "  D RifAlqDs        DS
008400000426  "  D  RifIva                        3A
008500000426  "  D  Aliq                          5S 2
008600000427  "  D*-------------
008700000427  "  D i               S              3  0
008800000427C1357D*-------------
008900030114C1611 * Parametri x calcolo Comunicazione annuale iva
009000030129C1611D NDC181DS      E DS
009100950405     C*----------------------------------------------------*
009200950405     C*                MAIN LINE PROGRAM
009300950405     C*----------------------------------------------------*
009400971212     C*
009500950606     C* Inizializzazione variabili
009600950606     C                   EXSR      INZVAR
009700950703     C*
009800950703     C* Riepilogo aliquote
009900950703     C                   EXSR      RIEALQ
010000950703     C*
010100950703     C* Riepilogo altre operazioni
010200030114     C                   EXSR      RIEAOP
010300030114C1611 *
010400030114  "   * Riepilogo dati 'Comunicazione annuale Iva'
010500030114C1611C                   EXSR      RIECAI
010600000426C1357C*
010700000426  "  C* Se la società gestisce la ventilazione ...
010800000426  "  C                   If        XScIvaVent = '1'
010900000511  "  C* e stampa registro acquisti ...
011000000426  "  C                             and TpR164 = '1'
011100000426  "  C* Riepilogo acquisti che concorrono alla ventilazione
011200000511  "  C                   ExSr      RieAcqVent
011300000426C1357C                   EndIf
011400900731     C*
011500130620     C                   EXSR      YAnnRiga
011600950606     C* Fine programma
011700950606     C                   EXSR      ENDPGM
011800950606     C*
011900950606     C/EJECT
012000950703     C************************************************************
012100950703     C* RIEPILOGO ALIQUOTE IVA
012200950703     C************************************************************
012300950703     C     RIEALQ        BEGSR
012400950704     C*
012500971212     C* Posizionamento su prima aliquota
012600971212     C                   move      soc164        ivasocieta
012700971212     C                   move      tpr164        ivatpreg
012800971212     C                   z-add     0             ivaaliq
012900971212     C                   ExSr      PosAlq1
013000950703     C*
013100971212     C* Lettura per ottenere prima aliquota
013200971212     C                   ExSr      LetAlq1
013300960418     C*
013400950704     C     *in22         doweq     *off
013500950704     C*
013600971212     C* Riposizionamento su aliquota anche con data inizio
013700971212     C                   ExSr      PosAlq2
013800950704     C*
013900971212     C* Lettura con aliquota
014000971212     C                   ExSr      LetAlq2
014100950712     C*
014200950712     C* Salvataggio aliquota
014300950712     C                   z-add     ivaaliq       savaliq
014400971212     C*
014500971212     C* Imposto data per controllo limiti
014600971212     C                   if        tpd164 = 'R'
014700020506C1550X****************** move      ivadtreg      wwdatactr
014800020506C1550C                   move      IVADtAnnot    wwdatactr
014900971212     C                   else
015000971212     C                   move      ivadtrif      wwdatactr
015100971212     C                   endif
015200950703     C*
015300950703     C     *in21         doweq     *off
015400971212     C     wwdatactr     andle     dtf164
015500950719     C*
015600950719     C* Segno
015700950719     C                   select
015800950719     C     ivatpreg      wheneq    '1'
015900950719     C     ivaavere      andeq     1
016000950719     C     ivaimporto    mult      -1            ivaimporto
016100950719     C     ivaimponib    mult      -1            ivaimponib
016200950719     C     ivatpreg      wheneq    '2'
016300950719     C     ivadare       andeq     1
016400950719     C     ivatpreg      oreq      '3'
016500950719     C     ivadare       andeq     1
016600950719     C     ivaimporto    mult      -1            ivaimporto
016700950719     C     ivaimponib    mult      -1            ivaimponib
016800950719     C                   endsl
016900950704     C*
017000950712     C* Accendo indicatore "stampa aliquota"
017100950712     C                   seton                                        61
017200950712     C*
017300950712     C* Stampa testata solo prima volta
017400950712     C     *in60         ifeq      *off
017500950712     C                   seton                                        60
017600950712     C                   EXSR      TESTA
017700950712     C                   select
017800950712     C     tpr164        wheneq    '1'
017900950712     C                   WRITE     TEST1A
018000950712     C     tpr164        wheneq    '2'
018100950712     C                   WRITE     TEST1V
018200950712     C     tpr164        wheneq    '3'
018300950712     C                   WRITE     TEST1C
018400950712     C                   endsl
018500950712     C                   endif
018600950712     C*
018700950704     C* Incremento totali aliquota
018800950712     C     ivaindetr     ifne      '1'
018900950703     C                   add       ivaimponib    q1imponnor
019000950703     C                   add       ivaimporto    q1imposnor
019100950703     C                   else
019200950703     C     tpr164        ifeq      '1'
019300950703     C                   add       ivaimponib    q1imponind
019400950703     C                   add       ivaimporto    q1imposind
019500950703     C                   endif
019600950703     C                   endif
019700950704     C*
019800971212     C* Lettura con aliquota
019900971212     C                   ExSr      LetAlq2
020000971212     C*
020100971212     C* Imposto data per controllo limiti
020200971212     C                   if        tpd164 = 'R'
020300020506C1550X****************** move      ivadtreg      wwdatactr
020400020506C1550C                   move      IVADtAnnot    wwdatactr
020500971212     C                   else
020600971212     C                   move      ivadtrif      wwdatactr
020700971212     C                   endif
020800950703     C*
020900950703     C                   enddo
021000950704     C*
021100950704     C* Elaborazione totali
021200950712     C     *in61         ifeq      *on
021300950712     C*
021400950704     C                   z-add     savaliq       q1aliq
021500950704     C* Reperisco descrizione aliquota
021600950704     C                   z-add     savaliq       rivaliq
021700950704     C     k3riv1        chain     anriv01l                           23
021800950704     C  N23              move      rivdesbrev    q1desali
021900950704     C   23              move      *blank        q1desali
022000950704     C* Totali aliquota
022100950704     C                   eval      q1totimpon=q1imponnor+q1imponind
022200950704     C                   eval      q1totimpos=q1imposnor+q1imposind
022300950704     C                   eval      q1totgen=q1totimpon+q1totimpos
022400950704     C* Incremento totali generali
022500950704     C                   eval      t1totimpon=t1totimpon+q1totimpon
022600950704     C                   eval      t1totimpos=t1totimpos+q1totimpos
022700950704     C                   eval      t1totgen=t1totgen+q1totgen
022800951215     C                   eval      t1imponnor=t1imponnor+q1imponnor
022900951215     C                   eval      t1imposnor=t1imposnor+q1imposnor
023000951215     C                   eval      t1imponind=t1imponind+q1imponind
023100951215     C                   eval      t1imposind=t1imposind+q1imposind
023200950704     C* Stampa totali aliquota
023300950706     C     *in39         ifeq      *on
023400950710     C     *in40         oreq      *on
023500950710     C     *in41         oreq      *on
023600950710     C                   EXSR      TESTA
023700950710     C                   select
023800950710     C     tpr164        wheneq    '1'
023900950710     C                   WRITE     TEST1A
024000950710     C     tpr164        wheneq    '2'
024100950710     C                   WRITE     TEST1V
024200950710     C     tpr164        wheneq    '3'
024300950710     C                   WRITE     TEST1C
024400950710     C                   endsl
024500950706     C                   endif
024600950710     C*
024700950710     C                   select
024800950710     C     tpr164        wheneq    '1'
024900950710     C                   WRITE     ALIQ1A
025000950710     C     tpr164        wheneq    '2'
025100950710     C                   WRITE     ALIQ1V
025200950710     C     tpr164        wheneq    '3'
025300950710     C                   WRITE     ALIQ1C
025400950710     C                   endsl
025500950710     C*
025600950712     C* Azzero totali aliquota
025700950704     C                   z-add     0             q1imponnor
025800950704     C                   z-add     0             q1imposnor
025900950704     C                   z-add     0             q1imponind
026000950704     C                   z-add     0             q1imposind
026100950712     C*
026200950712     C* Spengo indicatore "stampa aliquota"
026300950712     C                   setoff                                       61
026400950712     C*
026500950712     C                   endif
026600950704     C*
026700950704     C* Se non è cambiata l'aliquota
026800950704     C     ivaaliq       ifeq      savaliq
026900950704     C*
027000950704     C* Posizionamento per aliquota > aliquota precedente
027100971212     C                   ExSr      PosAlq1
027200950704     C*
027300950704     C* Lettura per ottenere nuova aliquota
027400971212     C                   ExSr      LetAlq1
027500950704     C*
027600950704     C                   endif
027700950704     C*
027800950704     C                   enddo
027900950704     C*
028000950707     C* Stampa totali generali aliquota
028100950712     C     *in60         ifeq      *on
028200950710     C*
028300950704     C     *in39         ifeq      *on
028400950710     C     *in40         oreq      *on
028500950710     C     *in41         oreq      *on
028600950710     C                   EXSR      TESTA
028700950710     C                   select
028800950710     C     tpr164        wheneq    '1'
028900950710     C                   WRITE     TEST1A
029000950710     C     tpr164        wheneq    '2'
029100950710     C                   WRITE     TEST1V
029200950710     C     tpr164        wheneq    '3'
029300950710     C                   WRITE     TEST1C
029400950710     C                   endsl
029500950704     C                   endif
029600950710     C*
029700950710     C                   select
029800950710     C     tpr164        wheneq    '1'
029900950710     C                   WRITE     TOTT1A
030000950710     C     tpr164        wheneq    '2'
030100950710     C                   WRITE     TOTT1V
030200950710     C     tpr164        wheneq    '3'
030300950710     C                   WRITE     TOTT1C
030400950710     C                   endsl
030500950710     C*
030600950710     C                   endif
030700950703     C*
030800950703     C                   ENDSR
030900950703     C/EJECT
031000971212     C************************************************************
031100971212     C* Posizionamento su nuova aliquota
031200971212     C************************************************************
031300971212     C     PosAlq1       BEGSR
031400971212     C*
031500971212     C* Con libro IVA ...
031600971212     C     lib164        ifne      *blank
031700971212     C                   move      lib164        ivalibro
031800971212     C                   if        tpd164 = 'R'
031900971212     C* Per data registrazione
032000020506C1550 * o per data annotazione
032100020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
032200020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
032300971212     C     k4iva9        setgt     ndiva09j
032400971212     C                   else
032500971212     C* Per data rif. IVA
032600971212     C     k4iva3        setgt     ndiva03j
032700971212     C                   endif
032800971212     C*
032900971212     C* ... senza libro IVA
033000971212     C                   else
033100971212     C                   if        tpd164 = 'R'
033200971212     C* Per data registrazione
033300020506C1550 * o per data annotazione
033400020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
033500020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
033600971212     C     k3iva0        setgt     ndiva10j
033700971212     C                   else
033800971212     C* Per data rif. IVA
033900971212     C     k3iva4        setgt     ndiva04j
034000971212     C                   endif
034100971212     C                   endif
034200971212     C*
034300971212     C                   ENDSR
034400971212     C/EJECT
034500971212     C************************************************************
034600971212     C* Lettura per ottenere nuova aliquota
034700971212     C************************************************************
034800971212     C     LetAlq1       BEGSR
034900971212     C*
035000971212     C                   do        *hival
035100971212     C*
035200971212     C* Con libro IVA ...
035300971212     C     lib164        ifne      *blank
035400971212     C                   if        tpd164 = 'R'
035500971212     C* Per data registrazione
035600020506C1550 * o per data annotazione
035700020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
035800020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
035900971212     C     k3iva9        reade     ndiva09j                               22
036000971212     C                   else
036100971212     C* Per data rif. IVA
036200971212     C     k3iva3        reade     ndiva03j                               22
036300971212     C                   endif
036400971212     C*
036500971212     C* ... senza libro IVA
036600971212     C                   else
036700971212     C                   if        tpd164 = 'R'
036800971212     C* Per data registrazione
036900020506C1550 * o per data annotazione
037000020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
037100020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
037200971212     C     k2iva0        reade     ndiva10j                               22
037300971212     C                   else
037400971212     C* Per data rif. IVA
037500971212     C     k2iva4        reade     ndiva04j                               22
037600971212     C                   endif
037700971212     C                   endif
037800971212     C*
037900971212     C* Verifica se è movimento definitivo e di contabilità 'CG'
038000971212     C                   EXSR      MOVDEF
038100971212     C   22              leave
038200971212     C  N24              leave
038300971212     C*
038400971212     C                   enddo
038500971212     C*
038600971212     C                   ENDSR
038700971212     C/EJECT
038800971212     C************************************************************
038900971212     C* Ri-Posizionamento su aliquota anche con data inizio
039000971212     C************************************************************
039100971212     C     PosAlq2       BEGSR
039200971212     C*
039300971212     C                   move      *blank        ivarifiva
039400971212     C*
039500971212     C* Con libro IVA ...
039600971212     C     lib164        ifne      *blank
039700971212     C                   if        tpd164 = 'R'
039800971212     C* Per data registrazione
039900020506C1550 * o per data annotazione
040000020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
040100020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
040200020506C1550X****************** move      dti164        ivadtreg
040300020506C1550C                   move      dti164        IVADtAnnot
040400971212     C     k6iva9        setll     ndiva09j
040500971212     C                   else
040600971212     C* Per data rif. IVA
040700971212     C                   move      dti164        ivadtrif
040800971212     C     k6iva3        setll     ndiva03j
040900971212     C                   endif
041000971212     C*
041100971212     C* ... senza libro IVA
041200971212     C                   else
041300971212     C                   if        tpd164 = 'R'
041400971212     C* Per data registrazione
041500020506C1550 * o per data annotazione
041600020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
041700020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
041800020506C1550X****************** move      dti164        ivadtreg
041900020506C1550C                   move      dti164        IVADtAnnot
042000971212     C     k5iva0        setll     ndiva10j
042100971212     C                   else
042200971212     C* Per data rif. IVA
042300971212     C                   move      dti164        ivadtrif
042400971212     C     k5iva4        setll     ndiva04j
042500971212     C                   endif
042600971212     C                   endif
042700971212     C*
042800971212     C                   ENDSR
042900971212     C/EJECT
043000971212     C************************************************************
043100971212     C* Lettura con aliquota
043200971212     C************************************************************
043300971212     C     LetAlq2       BEGSR
043400971212     C*
043500971212     C                   do        *hival
043600971212     C*
043700971212     C* Con libro IVA ...
043800971212     C     lib164        ifne      *blank
043900971212     C                   if        tpd164 = 'R'
044000971212     C* Per data registrazione
044100020506C1550 * o per data annotazione
044200020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
044300020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
044400971212     C     k4iva9        reade     ndiva09j                               21
044500971212     C                   else
044600971212     C* Per data rif. IVA
044700971212     C     k4iva3        reade     ndiva03j                               21
044800971212     C                   endif
044900971212     C*
045000971212     C* ... senza libro IVA
045100971212     C                   else
045200971212     C                   if        tpd164 = 'R'
045300971212     C* Per data registrazione
045400020506C1550 * o per data annotazione
045500020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
045600020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
045700971212     C     k3iva0        reade     ndiva10j                               21
045800971212     C                   else
045900971212     C* Per data rif. IVA
046000971212     C     k3iva4        reade     ndiva04j                               21
046100971212     C                   endif
046200971212     C                   endif
046300971212     C*
046400971212     C* Verifica se è movimento definitivo e di contabilità 'CG'
046500971212     C                   EXSR      MOVDEF
046600971212     C   21              leave
046700971212     C  N24              leave
046800971212     C*
046900971212     C                   enddo
047000971212     C*
047100971212     C                   ENDSR
047200971212     C/EJECT
047300950703     C************************************************************
047400950703     C* RIEPILOGO ALTRE OPERAZIONI IVA
047500950703     C************************************************************
047600950703     C     RIEAOP        BEGSR
047700950704     C*
047800971212     C* Posizionamento su primo riferimento iva
047900950704     C                   move      soc164        ivasocieta
048000950704     C                   move      tpr164        ivatpreg
048100950704     C                   z-add     0             ivaaliq
048200971212     C                   ExSr      PosRif1
048300950704     C*
048400971212     C* Lettura per ottenere primo riferimento iva
048500971212     C                   ExSr      LetRif1
048600950707     C*
048700950704     C     *in22         doweq     *off
048800950704     C*
048900971212     C* Riposizionamento su riferimento iva anche con data inizio
049000971212     C                   ExSr      PosRif2
049100950704     C*
049200971212     C* Lettura con riferimento iva
049300971212     C                   ExSr      LetRif2
049400950712     C*
049500971212     C* Salvataggio riferimento iva
049600950712     C                   move      ivarifiva     savrifiva
049700971212     C*
049800971212     C* Imposto data per controllo limiti
049900971212     C                   if        tpd164 = 'R'
050000020506C1550X****************** move      ivadtreg      wwdatactr
050100020506C1550C                   move      IVADtAnnot    wwdatactr
050200971212     C                   else
050300971212     C                   move      ivadtrif      wwdatactr
050400971212     C                   endif
050500950712     C*
050600950704     C     *in21         doweq     *off
050700971212     C     wwdatactr     andle     dtf164
050800950719     C*
050900950719     C* Segno
051000950719     C                   select
051100950719     C     ivatpreg      wheneq    '1'
051200950719     C     ivaavere      andeq     1
051300950719     C     ivaimponib    mult      -1            ivaimponib
051400950719     C     ivatpreg      wheneq    '2'
051500950719     C     ivadare       andeq     1
051600950719     C     ivatpreg      oreq      '3'
051700950719     C     ivadare       andeq     1
051800950719     C     ivaimponib    mult      -1            ivaimponib
051900950719     C                   endsl
052000950712     C*
052100971212     C* Accendo indicatore "stampa riferimento iva"
052200950712     C                   seton                                        71
052300950712     C*
052400950712     C* Stampo testata solo prima volta
052500950712     C     *in70         ifeq      *off
052600950712     C                   seton                                        70
052700950712     C     *in60         ifeq      *off
052800950712     C                   seton                                        60
052900950712     C                   EXSR      TESTA
053000950712     C                   endif
053100950712     C                   select
053200950712     C     tpr164        wheneq    '1'
053300950712     C                   WRITE     TEST2A
053400950712     C     tpr164        wheneq    '2'
053500950712     C                   WRITE     TEST2V
053600950712     C     tpr164        wheneq    '3'
053700950712     C                   WRITE     TEST2C
053800950712     C                   endsl
053900950712     C                   endif
054000950704     C*
054100971212     C* Incremento totali riferimento iva
054200950706     C                   add       ivaimponib    p2imponib
054300950704     C*
054400971212     C* Lettura con riferimento iva
054500971212     C                   ExSr      LetRif2
054600971212     C*
054700971212     C* Imposto data per controllo limiti
054800971212     C                   if        tpd164 = 'R'
054900020506C1550X****************** move      ivadtreg      wwdatactr
055000020506C1550C                   move      IVADtAnnot    wwdatactr
055100971212     C                   else
055200971212     C                   move      ivadtrif      wwdatactr
055300971212     C                   endif
055400950704     C*
055500950704     C                   enddo
055600950704     C*
055700950704     C* Elaborazione totali
055800950712     C     *in71         ifeq      *on
055900950712     C*
056000950706     C                   move      savrifiva     p2rifiva
056100971212     C* Reperisco descrizione riferimento iva
056200950704     C                   move      savrifiva     rivrifiva
056300950704     C                   z-add     0             rivaliq
056400950704     C     k4riv1        chain     anriv01l                           23
056500950706     C  N23              move      rivdesbrev    p2desrif
056600950706     C   23              move      *blank        p2desrif
056700950704     C* Incremento totale generale
056800950706     C                   eval      t2totimpon=t2totimpon+p2imponib
056900971212     C* Stampa totali riferimento iva
057000950704     C     *in39         ifeq      *on
057100950710     C     *in40         oreq      *on
057200950710     C     *in41         oreq      *on
057300950710     C                   EXSR      TESTA
057400950710     C                   select
057500950710     C     tpr164        wheneq    '1'
057600950710     C                   WRITE     TEST2A
057700950710     C     tpr164        wheneq    '2'
057800950710     C                   WRITE     TEST2V
057900950710     C     tpr164        wheneq    '3'
058000950710     C                   WRITE     TEST2C
058100950710     C                   endsl
058200950704     C                   endif
058300950710     C*
058400950710     C                   select
058500950710     C     tpr164        wheneq    '1'
058600950710     C                   WRITE     ALOP2A
058700950710     C     tpr164        wheneq    '2'
058800950710     C                   WRITE     ALOP2V
058900950710     C     tpr164        wheneq    '3'
059000950710     C                   WRITE     ALOP2C
059100950710     C                   endsl
059200950710     C*
059300971212     C* Azzero totale riferimento iva
059400950706     C                   z-add     0             p2imponib
059500950712     C*
059600971212     C* Spengo indicatore "stampa riferimento iva"
059700950712     C                   setoff                                       71
059800950704     C*
059900950712     C                   endif
060000950712     C*
060100971212     C* Se non è cambiato riferimento iva
060200950704     C     ivarifiva     ifeq      savrifiva
060300950704     C*
060400971212     C* Posizionamento per riferimento iva > riferimento iva precedente
060500971212     C                   ExSr      PosRif3
060600950704     C*
060700971212     C* Lettura per ottenere nuovo riferimento iva
060800971212     C                   ExSr      LetRif1
060900950704     C*
061000950704     C                   endif
061100950704     C*
061200950704     C                   enddo
061300950712     C*
061400950712     C     *in70         ifeq      *on
061500950704     C*
061600971212     C* Stampa totali generali riferimento iva
061700950704     C     *in39         ifeq      *on
061800950710     C     *in40         oreq      *on
061900950710     C     *in41         oreq      *on
062000950710     C                   EXSR      TESTA
062100950710     C                   select
062200950710     C     tpr164        wheneq    '1'
062300950710     C                   WRITE     TEST2A
062400950710     C     tpr164        wheneq    '2'
062500950710     C                   WRITE     TEST2V
062600950710     C     tpr164        wheneq    '3'
062700950710     C                   WRITE     TEST2C
062800950710     C                   endsl
062900950704     C                   endif
063000950710     C*
063100950710     C                   select
063200950710     C     tpr164        wheneq    '1'
063300950710     C                   WRITE     TOTT2A
063400950710     C     tpr164        wheneq    '2'
063500950710     C                   WRITE     TOTT2V
063600950710     C     tpr164        wheneq    '3'
063700950710     C                   WRITE     TOTT2C
063800950710     C                   endsl
063900950710     C*
064000950710     C                   endif
064100950703     C*
064200950703     C                   ENDSR
064300950703     C/EJECT
064400971212     C************************************************************
064500971212     C* Posizionamento per aliquota = 0 (su primo riferimento iva)
064600971212     C************************************************************
064700971212     C     PosRif1       BEGSR
064800971212     C*
064900971212     C* Con libro IVA ...
065000971212     C     lib164        ifne      *blank
065100971212     C                   move      lib164        ivalibro
065200971212     C                   if        tpd164 = 'R'
065300971212     C* Per data registrazione
065400020506C1550 * o per data annotazione
065500020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
065600020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
065700971212     C     k4iva9        setll     ndiva09j
065800971212     C                   else
065900971212     C* Per data rif. IVA
066000971212     C     k4iva3        setll     ndiva03j
066100971212     C                   endif
066200971212     C*
066300971212     C* ... senza libro IVA
066400971212     C                   else
066500971212     C                   if        tpd164 = 'R'
066600971212     C* Per data registrazione
066700020506C1550 * o per data annotazione
066800020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
066900020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
067000971212     C     k3iva0        setll     ndiva10j
067100971212     C                   else
067200971212     C* Per data rif. IVA
067300971212     C     k3iva4        setll     ndiva04j
067400971212     C                   endif
067500971212     C                   endif
067600971212     C*
067700971212     C                   ENDSR
067800971212     C/EJECT
067900971212     C************************************************************
068000971212     C* Lettura per ottenere un nuovo riferimento iva
068100971212     C************************************************************
068200971212     C     LetRif1       BEGSR
068300971212     C*
068400971212     C                   do        *hival
068500971212     C*
068600971212     C* Con libro IVA ...
068700971212     C     lib164        ifne      *blank
068800971212     C                   if        tpd164 = 'R'
068900971212     C* Per data registrazione
069000020506C1550 * o per data annotazione
069100020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
069200020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
069300971212     C     k4iva9        reade     ndiva09j                               22
069400971212     C                   else
069500971212     C* Per data rif. IVA
069600971212     C     k4iva3        reade     ndiva03j                               22
069700971212     C                   endif
069800971212     C*
069900971212     C* ... senza libro IVA
070000971212     C                   else
070100971212     C                   if        tpd164 = 'R'
070200971212     C* Per data registrazione
070300020506C1550 * o per data annotazione
070400020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
070500020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
070600971212     C     k3iva0        reade     ndiva10j                               22
070700971212     C                   else
070800971212     C* Per data rif. IVA
070900971212     C     k3iva4        reade     ndiva04j                               22
071000971212     C                   endif
071100971212     C                   endif
071200971212     C*
071300971212     C* Verifica se è movimento definitivo e di contabilità 'CG'
071400971212     C                   EXSR      MOVDEF
071500971212     C   22              leave
071600971212     C  N24              leave
071700971212     C                   enddo
071800971212     C*
071900971212     C                   ENDSR
072000971212     C/EJECT
072100971212     C************************************************************
072200971212     C* Ri-Posizionamento su nuovo riferimento iva
072300971212     C************************************************************
072400971212     C     PosRif2       BEGSR
072500971212     C*
072600971212     C                   z-add     0             ivaaliq
072700971212     C*
072800971212     C* Con libro IVA ...
072900971212     C     lib164        ifne      *blank
073000971212     C                   if        tpd164 = 'R'
073100971212     C* Per data registrazione
073200020506C1550 * o per data annotazione
073300020506  "   * se regime iva NON trasport. mista, R=Data registrazione
073400020506C1550 * e dt annot = dt reg, altrimenti R=Data annotazione
073500020506C1550X****************** move      dti164        ivadtreg
073600020506C1550C                   move      dti164        IVADtAnnot
073700971212     C     k6iva9        setll     ndiva09j
073800971212     C                   else
073900971212     C* Per data rif. IVA
074000971212     C                   move      dti164        ivadtrif
074100971212     C     k6iva3        setll     ndiva03j
074200971212     C                   endif
074300971212     C*
074400971212     C* ... senza libro IVA
074500971212     C                   else
074600971212     C                   if        tpd164 = 'R'
074700971212     C* Per data registrazione
074800020506C1550 * o per data annotazione
074900020506  "   * se regime iva NON trasport. mista, R=Data registrazione
075000020506C1550 * e dt annot = dt reg, altrimenti R=Data annotazione
075100020506C1550X****************** move      dti164        ivadtreg
075200020506C1550C                   move      dti164        IVADtAnnot
075300971212     C     k5iva0        setll     ndiva10j
075400971212     C                   else
075500971212     C* Per data rif. IVA
075600971212     C                   move      dti164        ivadtrif
075700971212     C     k5iva4        setll     ndiva04j
075800971212     C                   endif
075900971212     C                   endif
076000971212     C*
076100971212     C                   ENDSR
076200971212     C/EJECT
076300971212     C************************************************************
076400971212     C* Lettura anche con riferimento iva
076500971212     C************************************************************
076600971212     C     LetRif2       BEGSR
076700971212     C*
076800971212     C                   do        *hival
076900971212     C*
077000971212     C* Con libro IVA ...
077100971212     C     lib164        ifne      *blank
077200971212     C                   if        tpd164 = 'R'
077300971212     C* Per data registrazione
077400020506C1550 * o per data annotazione
077500020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
077600020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
077700971212     C     k5iva9        reade     ndiva09j                               21
077800971212     C                   else
077900971212     C* Per data rif. IVA
078000971212     C     k5iva3        reade     ndiva03j                               21
078100971212     C                   endif
078200971212     C*
078300971212     C* ... senza libro IVA
078400971212     C                   else
078500971212     C                   if        tpd164 = 'R'
078600971212     C* Per data registrazione
078700020506C1550 * o per data annotazione
078800020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
078900020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
079000971212     C     k4iva0        reade     ndiva10j                               21
079100971212     C                   else
079200971212     C* Per data rif. IVA
079300971212     C     k4iva4        reade     ndiva04j                               21
079400971212     C                   endif
079500971212     C                   endif
079600971212     C*
079700971212     C* Verifica se è movimento definitivo e di contabilità 'CG'
079800971212     C                   EXSR      MOVDEF
079900971212     C   21              leave
080000971212     C  N24              leave
080100971212     C*
080200971212     C                   enddo
080300971212     C*
080400971212     C                   ENDSR
080500971212     C/EJECT
080600971212     C************************************************************
080700971212     C* Posizionamento su nuovo riferimento iva
080800971212     C************************************************************
080900971212     C     PosRif3       BEGSR
081000971212     C*
081100971212     C* Con libro IVA ...
081200971212     C     lib164        ifne      *blank
081300971212     C                   move      lib164        ivalibro
081400971212     C                   if        tpd164 = 'R'
081500971212     C* Per data registrazione
081600020506C1550 * o per data annotazione
081700020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
081800020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
081900971212     C     k5iva9        setgt     ndiva09j
082000971212     C                   else
082100971212     C* Per data rif. IVA
082200971212     C     k5iva3        setgt     ndiva03j
082300971212     C                   endif
082400971212     C*
082500971212     C* ... senza libro IVA
082600971212     C                   else
082700971212     C                   if        tpd164 = 'R'
082800971212     C* Per data registrazione
082900020506C1550 * o per data annotazione
083000020506  "   * (se regime iva NON trasport. mista, R=Data registrazione
083100020506C1550 *  e dt annot = dt reg, altrimenti R=Data annotazione)
083200971212     C     k4iva0        setgt     ndiva10j
083300971212     C                   else
083400971212     C* Per data rif. IVA
083500971212     C     k4iva4        setgt     ndiva04j
083600971212     C                   endif
083700971212     C                   endif
083800971212     C*
083900971212     C                   ENDSR
084000971212     C/EJECT
084100960418     C************************************************************
084200960515     C* CONTROLLA MOVIMENTO DEFINITIVO E DI CONTABILITA' CG
084300960418     C************************************************************
084400960418     C     MOVDEF        BEGSR
084500960418     C*
084600960418     C                   setoff                                       24
084700960418     C*
084800960418     C                   move      ivasys        movsys
084900960418     C                   move      ivanrasreg    movnrasreg
085000960418     C                   move      ivanrrigam    movnrrigam
085100960418     C     k3mov1        chain     ndmov01l                           24
085200960515     C                   if        *in24 = *off and
085300960515     C                             (movtpregz <> 'D' or movctb <> ctb164)
085400960418     C                   seton                                        24
085500960418     C                   endif
085600960418     C*
085700960418     C                   ENDSR
085800960418     C/EJECT
085900030114C1611 ************************************************************
086000030114  "   * Riepilogo 'Comunicazione Annuale Iva'
086100030114  "   ************************************************************
086200030114  "  C     RieCAI        Begsr
086300030114  "   *
086400030114  "   * se richiesto operazioni attive/passive a dettaglio di reg.
086500030115  "  C                   if        OpAPDet164 = *on
086600030114  "   *
086700030129  "  C                   reset                   NDC181DS
086800030129  "  C                   move      Soc164        Soc181
086900030129  "  C                   move      Ctb164        Ctb181
087000030129  "  C                   move      TpR164        TpReg181
087100030129  "  C                   move      Lib164        Libro181
087200030129  "  C                   move      DtI164        DtI181
087300050412C1611C                   move      DtF164        DtF181
087400050412C1814C                   move      TipDat2164    TipDat2181
087500050412C1611 *       solo calcolo (non stampa)
087600030129  "  C                   move      *off          Stampa181
087700030114  "   *       Chiusura in LR
087800030129  "  C                   move      'L'           End181
087900030129  "  C                   movel     NDC181DS      Kpjbu
088000030129  "  C                   call      'NDC181R'
088100030114  "  C                   parm                    Kpjba
088200030129  "  C                   movel     Kpjbu         NDC181DS
088300030114  "   *
088400030114  "   *
088500030115  "   * STAMPA dati per comunicaz. annuale iva se esiste almeno un importo
088600030129  "  C                   if        OpAtt181   <> 0  or
088700030129  "  C                             OpAttNI181 <> 0  or
088800030129  "  C                             OpAttEs181 <> 0  or
088900030129  "  C                             OpAttCI181 <> 0  or
089000030129  "  C                             OpPas181   <> 0  or
089100030129  "  C                             OpPasNI181 <> 0  or
089200030129  "  C                             OpPasEs181 <> 0  or
089300030129  "  C                             OpPasAI181 <> 0  or
089400030129  "  C                             TotVen181  <> 0  or
089500030129  "  C                             TotAcq181  <> 0
089600030115  "   *  testata
089700030115  "  C                   eval      *in60 = *on
089800030115  "  C                   EXSR      TESTA
089900030128  "   *
090000030128  "   * importi e segno operazioni attive/passive
090100030128  "   * (segno: se l'importo = 0 evidenzio il segno naturale dell'operaz.)
090200030129  "  C                   if        OpAtt181   <  0
090300030129  "  C                   eval      P4OpAtt    = OpAtt181   * -1
090400030128  "  C                   eval      P4OpAttS   = XScDare
090500030128  "  C                   else
090600030129  "  C                   eval      P4OpAtt    = OpAtt181
090700030128  "  C                   eval      P4OpAttS   = XScAvere
090800030128  "  C                   endif
090900030129  "  C                   if        OpAttNI181 <  0
091000030129  "  C                   eval      P4OpAttNI  = OpAttNI181 * -1
091100030128  "  C                   eval      P4OpAttNIS = XScDare
091200030128  "  C                   else
091300030129  "  C                   eval      P4OpAttNI  = OpAttNI181
091400030128  "  C                   eval      P4OpAttNIS = XScAvere
091500030128  "  C                   endif
091600030129  "  C                   if        OpAttEs181 <  0
091700030129  "  C                   eval      P4OpAttEs  = OpAttEs181 * -1
091800030128  "  C                   eval      P4OpAttEsS = XScDare
091900030128  "  C                   else
092000030129  "  C                   eval      P4OpAttEs  = OpAttEs181
092100030128  "  C                   eval      P4OpAttEsS = XScAvere
092200030128  "  C                   endif
092300030129  "  C                   if        OpAttCI181 <  0
092400030129  "  C                   eval      P4OpAttCI  = OpAttCI181 * -1
092500030128  "  C                   eval      P4OpAttCIS = XScDare
092600030128  "  C                   else
092700030129  "  C                   eval      P4OpAttCI  = OpAttCI181
092800030128  "  C                   eval      P4OpAttCIS = XScAvere
092900030128  "  C                   endif
093000030129  "  C                   if        TotVen181  <  0
093100030129  "  C                   eval      P4TotVen   = TotVen181  * -1
093200030128  "  C                   eval      P4TotVenS  = XScDare
093300030128  "  C                   else
093400030129  "  C                   eval      P4TotVen   = TotVen181
093500030128  "  C                   eval      P4TotVenS  = XScAvere
093600030128  "  C                   endif
093700030129  "  C                   if        OpPas181   <=  0
093800030129  "  C                   eval      P4OpPas    = OpPas181   * -1
093900030128  "  C                   eval      P4OpPasS   = XScDare
094000030128  "  C                   else
094100030129  "  C                   eval      P4OpPas    = OpPas181
094200030128  "  C                   eval      P4OpPasS   = XScAvere
094300030128  "  C                   endif
094400030129  "  C                   if        OpPasNI181 <=  0
094500030129  "  C                   eval      P4OpPasNI  = OpPasNI181 * -1
094600030128  "  C                   eval      P4OpPasNIS = XScDare
094700030128  "  C                   else
094800030129  "  C                   eval      P4OpPasNI  = OpPasNI181
094900030128  "  C                   eval      P4OpPasNIS = XScAvere
095000030128  "  C                   endif
095100030129  "  C                   if        OpPasEs181 <=  0
095200030129  "  C                   eval      P4OpPasEs  = OpPasEs181 * -1
095300030128  "  C                   eval      P4OpPasEsS = XScDare
095400030128  "  C                   else
095500030129  "  C                   eval      P4OpPasEs  = OpPasEs181
095600030128  "  C                   eval      P4OpPasEsS = XScAvere
095700030128  "  C                   endif
095800030129  "  C                   if        OpPasAI181 <=  0
095900030129  "  C                   eval      P4OpPasAI  = OpPasAI181 * -1
096000030128  "  C                   eval      P4OpPasAIS = XScDare
096100030128  "  C                   else
096200030129  "  C                   eval      P4OpPasAI  = OpPasAI181
096300030128  "  C                   eval      P4OpPasAIS = XScAvere
096400030128  "  C                   endif
096500030129  "  C                   if        TotAcq181  <=  0
096600030129  "  C                   eval      P4TotAcq   = TotAcq181  * -1
096700030128  "  C                   eval      P4TotAcqS  = XScDare
096800030128  "  C                   else
096900030129  "  C                   eval      P4TotAcq   = TotAcq181
097000030128  "  C                   eval      P4TotAcqS  = XScAvere
097100030128  "  C                   endif
097200030128  "   *
097300030128  "   *  stampa
097400030128  "  C                   select
097500030128  "  C                   when      TpR164='1'
097600030115  "  C                   write     Test4A
097700030115  "  C                   when      TpR164='2'
097800030115  "  C                   write     Test4V
097900030115  "  C                   when      TpR164='3'
098000030115  "  C                   write     Test4C
098100030115  "  C                   endsl
098200030115  "   *
098300030115  "  C                   endif
098400030114  "   *
098500030114  "  C                   endif
098600030114  "   *
098700030115C1611C     XRieCAI       Endsr
098800000426C1357C************************************************************
098900000511  "  C* Riepilogo acquisti che concorrono alla ventilazione
099000000426  "  C************************************************************
099100000511C1357C     RieAcqVent    BEGSR
099200000427     C*
099300000518     C* (solo se ho già stampato qualcosa)
099400000518     C                   If        *In60 = *On or *In70 = *On
099500000518     C*
099600000426     C                   Clear                   NDC267DS
099700000426     C                   Eval      Societa267 = Soc164
099800000426     C                   Eval      Ctb267 = Ctb164
099900000426     C                   Eval      DataI267 = DtI164
100000000426     C                   Eval      DataF267 = DtF164
100100000426     C                   Eval      Libro267 = Lib164
100200000426     C                   Eval      TpData267 = TpD164
100300000426     C*
100400000426     C                   Call      'NDC267R'
100500000426     C                   Parm                    NDC267DS
100600000426     C*
100700000426     C                   If        Err267 = *Off
100800000427     C* valorizza skiere di work
100900000502     C                   MoveL     DatiAcq267    DsDati
101000000427     C*
101100000427     C* azzera totali aliquote
101200000427     C                   Clear                   T3TotImpon
101300000427     C                   Clear                   T3TotImpos
101400000427     C                   Clear                   T3TotGen
101500000427     C                   Move      *Off          $StpTesVen        1
101600000426     C*
101700000427     C* ciclo su skiere aliquote
101800000427     C                   Do        $DimSk        i
101900000427     C*
102000120208D2719C                   If        SkRifAlq(i) = *hival
102100120208  "  C                   Leave
102200120208  "  C                   Endif
102300120208D2719C*
102400000426     C                   MoveL     SkRifAlq(i)   RifAlqDs
102500000427     C                   If        Aliq <> 0 and Aliq <> *HiVal
102600000427     C* stampa testata solo la prima volta
102700000427     C                   If        $StpTesVen = *Off
102800000427     C                   Eval      $StpTesVen = *On
102900000427     C                   If        *In39 = *On
103000000427     C                   ExSr      Testa
103100000427     C                   EndIf
103200000427     C                   Write     Test3A
103300000427     C                   EndIf
103400000427     C* valorizza campi dettaglio aliquote
103500000427     C                   Eval      Q3Aliq = Aliq
103600000427     C                   Eval      Q3TotImpon = SkImponib(i)
103700000427     C                   Eval      Q3TotImpos = SkImporto(i)
103800000427     C                   Eval      Q3TotGen = SkImponib(i) + SkImporto(i)
103900000427     C* (descrizione aliquota)
104000000427     C                   Eval      RIVAliq = Aliq
104100000427     C     k3riv1        Chain     ANRIV01L                           23
104200000427     C  N23              MoveL     RIVDesBrev    Q3DesAli
104300000427     C   23              Clear                   Q3DesAli
104400000427     C* stampa dettaglio aliquote
104500000427     C                   If        *in39 = *On
104600000427     C                   ExSr      Testa
104700000427     C                   Write     Test3A
104800000427     C                   EndIf
104900000427     C                   Write     Aliq3A
105000000427     C*
105100000427     C* incrementa totali
105200000427     C                   Eval      T3TotImpon = T3TotImpon + SkImponib(i)
105300000427     C                   Eval      T3TotImpos = T3TotImpos + SkImporto(i)
105400000427     C                   Eval      T3TotGen = T3TotGen +
105500000427     C                                        SkImponib(i) + SkImporto(i)
105600000502     C*
105700000502     C* fine aliquote (non stampa op. non imponibili)
105800000426     C                   Else
105900000426     C                   Leave
106000000426     C                   EndIf
106100000427     C*
106200000426     C                   EndDo
106300000426     C*
106400000426     C* stampa totali aliquote
106500001116C1438C                   If        $StpTesVen = *On
106600000427     C                   If        *in39 = *On
106700000427     C                   ExSr      Testa
106800000427     C                   Write     Test3A
106900000427     C                   EndIf
107000000427     C                   Write     TotT3A
107100001116C1438C                   EndIf
107200000426     C*
107300000426     C                   EndIf
107400000518     C*
107500000518     C                   EndIf
107600000426     C*
107700000426C1357C                   ENDSR
107800950710     C************************************************************
107900950710     C* STAMPA TESTATA
108000950710     C************************************************************
108100950710     C     TESTA         BEGSR
108200950710     C*
108300950710     C     bol164        ifeq      *off
108400020124c1529C     int164        andeq     *off
108500950710     C                   select
108600950710     C     tpr164        wheneq    '1'
108700950710     C                   WRITE     TESTA1A
108800950710     C     tpr164        wheneq    '2'
108900950710     C                   WRITE     TESTA1V
109000950710     C     tpr164        wheneq    '3'
109100950710     C                   WRITE     TESTA1C
109200950710     C                   endsl
109300950710     C                   endif
109400020124     c*
109500020124     c* se richieste intestazioni(indipendentemente dal bollato)
109600020124C1529C                   if        int164 = *on
109700020124  "  c* se bollato stampo intestazione memorizzata su param. modulo
109800020124  "  c                   eval      T0riga1 =rg1099
109900040308C1529c* anno/pagina
110000030218c1615x***  anno099       ifne      *blanks
110100030218  "  x*                  eval      %subst(T0riga1:122:4) = anno099
110200030218  "  x*                  eval      %subst(T0riga1:126:1) = '/'
110300030218  "  x*                  endif
110400030218  "  x*                  move      NumeroPag     T0riga1
110500040308C1615c                   move      numeropag     numpag            6
110600030218C1615C                   eval      P1AnnoPag= Anno099+ '/' + numpag
110700040308C1529c                   eval      T0riga2 =rg2099
110800020124  "  c                   eval      T0riga3 =rg3099
110900020124  "  c                   eval      T0riga4 =rg4099
111000020124  "  c                   select
111100020124  "  C     tpr164        wheneq    '1'
111200020124  "  c                   WRITE     Testa0A
111300020124  "  C     tpr164        wheneq    '2'
111400020124  "  c                   WRITE     Testa0V
111500020124  "  C     tpr164        wheneq    '3'
111600020124  "  c                   WRITE     Testa0C
111700020124  "  c                   endsl
111800020124  "  C                   ADD       1             NumeroPag
111900020124c1529c                   endif
112000950710     C*
112100950710     C                   select
112200950710     C     tpr164        wheneq    '1'
112300950710     C                   WRITE     TESTA2A
112400950710     C     tpr164        wheneq    '2'
112500950710     C                   WRITE     TESTA2V
112600950710     C     tpr164        wheneq    '3'
112700950710     C                   WRITE     TESTA2C
112800950710     C                   endsl
112900950710     C*
113000950710     C                   setoff                                       39
113100950710     C                   setoff                                       40
113200950710     C                   setoff                                       41
113300950710     C*
113400950710     C                   ENDSR
113500950710      /EJECT
113600950606     C************************************************************
113700950606     C* FINE PROGRAMMA
113800950606     C************************************************************
113900950606     C     ENDPGM        BEGSR
114000020124     c*
114100030422c1651c* aggiornamento numero protocollo e numero di pagina
114200030508  "  c                   exsr      AggTab008
114300030508c1651c                   z-add     NumeroPag     Pag164            6 0
114400950606     C*
114500950703     C                   MOVEL     C164DS        KPJBU
114600951201     C     end164        ifeq      'R'
114700951201     C                   SETON                                        RT
114800951201     C                   else
114900951201     C                   SETON                                        LR
115000951201     C                   endif
115100950606     C                   RETURN
115200950606     C*
115300950606     C                   ENDSR
115400950606     C/EJECT
115500030422c1651C************************************************************
115600030422     C* Aggiornamento ultimo numero protocollo e ultimo numero di
115700030422     c* pagina
115800030422c1651C************************************************************
115900030422     C     AggTab008     BEGSR
116000030422     c*
116100030422     C* aggiorno con ultimo numero
116200030422     C* di pagina il parametro modulo dove è memorizzata
116300030422     c* se chiesto il bollato
116400030422     c                   if        bol164=*on  and
116500030422     c* e se non ci sono stati errori
116600030422     c                             err164<>*on
116700030422     c* aggancio tabella 008 PER LIBRO
116800030422     c                   reset                   xatbds
116900030422     C                   MOVE      soc164        XTBAZI
117000030422     C                   MOVE      '008'         XTBcod
117100030422     c                   eval      xtbkey = '00000000000' + tpr164 + lib164
117200030422     C                   MOVE      '1'           XTBRIC
117300030422     C                   MOVE      '1'           XTBALC
117400030422     C                   CALLB     'XATB'
117500030422     c                   parm                    xatbds
117600030422     c                   eval      DesBrv008=   Dbr164
117700030422     C*
117800030422     c* se richieste intestazioni
117900030422     c                   if        int164= *on  and
118000030422     C* e numerazione per libro
118100030422     c                             numeraz045=*blanks
118200030422     C* Aggiorno ultimo numero di pagina
118300030422     c                   z-add     NumeroPag     Pagina008
118400030422     C                   ENDIF
118500030422     C* aggiorno numero protocollo /data SOLO SE <> 0, perchè se uguale
118600030422     c* significa che sono stati chiesti solo i totali o una ristampa e
118700030422     C* lascio il numero memorizzato che c'è
118800030422     c                   if        NrReg164 <> 0
118900030422     c                   z-add     NrReg164      NrReg008
119000030422     c                   move      DtReG164      DtReg008
119100030422     C                   endif
119200030422     c*
119300030422     c                   eval      xtbuni = ang008ds
119400030422     C                   MOVE      '4'           XTBric
119500030422     C                   CALLB     'XATB'
119600030422     c                   parm                    xatbds
119700030422     c                   endif
119800030422     C*
119900030505     c* se chiesto il bollato
120000030505     c                   if        bol164=*on  and
120100030505     c* e se non ci sono stati errori
120200030505     c                             err164<>*on and
120300030422     c* se richieste intestazioni
120400030505     c                             int164= *on  and
120500030422     C* e numerazione per registro
120600030422     c                             numeraz045='1'
120700030422     c* aggancio tabella 008 PER REGISTRO
120800030422     c                   reset                   xatbds
120900030422     C                   MOVE      soc164        XTBAZI
121000030422     C                   MOVE      '008'         XTBcod
121100030422     c                   eval      xtbkey = '00000000000' + tpr164 + '   '
121200030422     C                   MOVE      '1'           XTBRIC
121300030422     C                   MOVE      '1'           XTBALC
121400030422     C                   CALLB     'XATB'
121500030422     c                   parm                    xatbds
121600030422     C* Aggiorno ultimo numero di pagina
121700030422     c                   z-add     NumeroPag     Pagina008
121800030505     c* Descrizione
121900030505     c                   select
122000030505     c                   when      tpr164='1'
122100030505     c                   eval      desbrv008= 'Libro iva Acquisti'
122200030505     c                   when      tpr164='2'
122300030505     c                   eval      desbrv008= 'Libro iva Vendite'
122400030505     c                   when      tpr164='3'
122500030505     c                   eval      desbrv008 = 'Libro iva Corrisp. '
122600030505     c                   endsl
122700030422     c*
122800030422     c                   eval      xtbuni = ang008ds
122900030422     C                   MOVE      '4'           XTBric
123000030422     C                   CALLB     'XATB'
123100030422     c                   parm                    xatbds
123200030422     C                   endif
123300030422     C*
123400030422c1651C                   ENDSR
123500950405     C************************************************************
123600900731     C* Definizioni e inizializzazioni
123700950405     C************************************************************
123800940831     C     *INZSR        BEGSR
123900000118C1309C     *DMY          Move      Udate         ZDateISO
124000000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
124100900731     C*
124200900731     C     *ENTRY        PLIST
124300900731     C                   PARM                    KPJBA
124400941104     C*
124500940831     C                   ENDSR
124600971212     C/EJECT
124700950606     C************************************************************
124800950606     C* INIZIALIZZAZIONE VARIABILI
124900950606     C************************************************************
125000950606     C     INZVAR        BEGSR
125100950606     C*
125200950703     C                   MOVEL     KPJBU         C164DS
125300130620>>>>>C* Faccio lavorare il programma sempre per data liquidazione.
125400130620>>>>>C                   EVAL      tpd164 = 'F'
125500950627     C*
125600971212     C* klist per NDIVA03L
125700950704     C     k6iva3        KLIST
125800950704     C                   KFLD                    ivasocieta
125900950704     C                   KFLD                    ivatpreg
126000950704     C                   KFLD                    ivalibro
126100950704     C                   KFLD                    ivaaliq
126200950704     C                   KFLD                    ivarifiva
126300950704     C                   KFLD                    ivadtrif
126400950704     C     k5iva3        KLIST
126500950704     C                   KFLD                    ivasocieta
126600950704     C                   KFLD                    ivatpreg
126700950704     C                   KFLD                    ivalibro
126800950704     C                   KFLD                    ivaaliq
126900950704     C                   KFLD                    ivarifiva
127000950703     C     k4iva3        KLIST
127100950630     C                   KFLD                    ivasocieta
127200950630     C                   KFLD                    ivatpreg
127300950630     C                   KFLD                    ivalibro
127400950703     C                   KFLD                    ivaaliq
127500950703     C     k3iva3        KLIST
127600950703     C                   KFLD                    ivasocieta
127700950703     C                   KFLD                    ivatpreg
127800950703     C                   KFLD                    ivalibro
127900950703     C*
128000971212     C* klist per NDIVA04L
128100950704     C     k5iva4        KLIST
128200950704     C                   KFLD                    ivasocieta
128300950704     C                   KFLD                    ivatpreg
128400950704     C                   KFLD                    ivaaliq
128500950704     C                   KFLD                    ivarifiva
128600950704     C                   KFLD                    ivadtrif
128700950704     C     k4iva4        KLIST
128800950704     C                   KFLD                    ivasocieta
128900950704     C                   KFLD                    ivatpreg
129000950704     C                   KFLD                    ivaaliq
129100950704     C                   KFLD                    ivarifiva
129200950703     C     k3iva4        KLIST
129300950630     C                   KFLD                    ivasocieta
129400950630     C                   KFLD                    ivatpreg
129500950703     C                   KFLD                    ivaaliq
129600950703     C     k2iva4        KLIST
129700950703     C                   KFLD                    ivasocieta
129800950703     C                   KFLD                    ivatpreg
129900971212     C*
130000971212     C* klist per NDIVA09L
130100971212     C     k6iva9        KLIST
130200971212     C                   KFLD                    ivasocieta
130300971212     C                   KFLD                    ivatpreg
130400971212     C                   KFLD                    ivalibro
130500971212     C                   KFLD                    ivaaliq
130600971212     C                   KFLD                    ivarifiva
130700020506C1550X****************** KFLD                    ivadtreg
130800020506C1550C                   KFLD                    IVADtAnnot
130900971212     C     k5iva9        KLIST
131000971212     C                   KFLD                    ivasocieta
131100971212     C                   KFLD                    ivatpreg
131200971212     C                   KFLD                    ivalibro
131300971212     C                   KFLD                    ivaaliq
131400971212     C                   KFLD                    ivarifiva
131500971212     C     k4iva9        KLIST
131600971212     C                   KFLD                    ivasocieta
131700971212     C                   KFLD                    ivatpreg
131800971212     C                   KFLD                    ivalibro
131900971212     C                   KFLD                    ivaaliq
132000971212     C     k3iva9        KLIST
132100971212     C                   KFLD                    ivasocieta
132200971212     C                   KFLD                    ivatpreg
132300971212     C                   KFLD                    ivalibro
132400971212     C*
132500971212     C* klist per NDIVA10L
132600971212     C     k5iva0        KLIST
132700971212     C                   KFLD                    ivasocieta
132800971212     C                   KFLD                    ivatpreg
132900971212     C                   KFLD                    ivaaliq
133000971212     C                   KFLD                    ivarifiva
133100020506C1550X****************** KFLD                    ivadtreg
133200020506C1550C                   KFLD                    IVADtAnnot
133300971212     C     k4iva0        KLIST
133400971212     C                   KFLD                    ivasocieta
133500971212     C                   KFLD                    ivatpreg
133600971212     C                   KFLD                    ivaaliq
133700971212     C                   KFLD                    ivarifiva
133800971212     C     k3iva0        KLIST
133900971212     C                   KFLD                    ivasocieta
134000971212     C                   KFLD                    ivatpreg
134100971212     C                   KFLD                    ivaaliq
134200971212     C     k2iva0        KLIST
134300971212     C                   KFLD                    ivasocieta
134400971212     C                   KFLD                    ivatpreg
134500960418     C*
134600971212     C* klist per NDMOV01L
134700960418     C     K3MOV1        KLIST
134800960418     C                   KFLD                    movsys
134900960418     C                   KFLD                    movnrasreg
135000960418     C                   KFLD                    movnrrigam
135100950703     C*
135200971212     C* klist per ANRIV01L
135300950703     C     k3riv1        KLIST
135400950703     C                   KFLD                    rivsocieta
135500950703     C                   KFLD                    rivtpreg
135600950703     C                   KFLD                    rivaliq
135700950703     C     k4riv1        KLIST
135800950703     C                   KFLD                    rivsocieta
135900950703     C                   KFLD                    rivtpreg
136000950703     C                   KFLD                    rivaliq
136100950703     C                   KFLD                    rivrifiva
136200950703     C*
136300950606     C* Reperimento azienda
136400950703     C                   MOVE      SOC164        SOCXSC
136500950606     C                   MOVEL     'SOC001'      TIPXSC
136600950606     C                   EXSR      REPSOC
136700950606     C     RTNXSC        IFNE      '1'
136800950606     C                   MOVEL     XSOCDS        SOC001
136900950606     C                   ELSE
137000950703     C                   MOVE      *ON           ERR164
137100950606     C                   EXSR      ENDPGM
137200950606     C                   ENDIF
137300030418C1651C*
137400030418  "  C* Reperimento parametro IVA (anagrafiche iva)
137500030418  "  C*
137600030418  "  C                   CALLB     'XPAR'
137700030418  "  C                   parm                    xscsoc
137800030418  "  C                   parm      'IVA     '    xpapar            8
137900030418  "  C                   parm                    xpaout
138000030418  "  C                   parm      *OFF          xpaerr            1
138100030418  "  C                   parm      ' '           xparic            1
138200030418  "  C     xpaerr        ifeq      *ON
138300030418  "  C                   MOVE      *ON           ERR164
138400030418  "  C                   EXSR      ENDPGM
138500030418  "  C                   else
138600030418  "  C                   movel     xpaout        ANP045DS
138700030418c1651C                   endif
138800950704     C*
138900950704     C* Valorizzazione campi univoci testate
139000950710     C                   MOVEL     KNSIF         NOMSIF
139100950704     C                   MOVEL     XSCDSI        NOMDIT
139200950704     C                   MOVEL     DSPGM         NOMPGM
139300950710     C* Reperisco nome sistema informatico
139400950710     C                   CALL      'XNETA'                              21
139500950710     C                   PARM                    NOMSYS
139600950707     C                   move      XSCCDF        codfis
139700950707     C                   move      lib164        t2libro
139800950707     C                   move      dbr164        t2desreg
139900040308C1309x******dmy          move      dti164        t2datai
140000000118C1309C     *JobRun       move      dti164        t2datai
140100040308C1309x******dmy          move      dtf164        t2dataf
140200000118C1309C     *JobRun       move      dtf164        t2dataf
140300950630     C*
140400950704     C* Campi di lavoro
140500950703     C     *like         define    ivaaliq       savaliq
140600950703     C     *like         define    ivarifiva     savrifiva
140700950703     C*
140800950707     C* Imposto klist x ANRIV
140900950703     C                   move      soc164        rivsocieta
141000950703     C                   move      tpr164        rivtpreg
141100020124     C*
141200020124c1529C* se richiesta intestazione  reperisco intestazione memorizza
141300020124  "  C* ta sul parametro modulo 'LIBRIIVA'
141400020124  "  C                   if        int164=*on
141500020124  "  c                   select
141600020124  "  c                   when      tpr164='1'
141700020124  "  c                   eval      xpapar = 'LIBRIVAA'
141800020124  "  c                   when      tpr164='2'
141900020124  "  c                   eval      xpapar = 'LIBRIVAV'
142000020124  "  C                   other
142100020124  "  c                   eval      xpapar = 'LIBRIVAC'
142200020124  "  c                   endsl
142300020124  "  C                   CALLB     'XPAR'
142400020124  "  C                   parm                    xscsoc
142500020124  "  C                   parm                    xpapar            8
142600020124  "  C                   parm                    xpaout
142700020124  "  C                   parm      *OFF          xpaerr            1
142800020124  "  C                   parm                    xparic            1
142900020124  "  c                   movel     xpaout        ndc099ds
143000020125  "  c* imposto pagina da cui partire
143100020125  "  c* il numero pagina da cui partire glielo passa NDC163R
143200020124  "  C                   if        xpaerr<>*on
143300020125  "  c                   z-add     Pag164        NumeroPag         6 0
143400020124  "  c                   else
143500020124  "  c* arriva messaggio di errore da xpar all'operatore
143600020124  "  c                   eval      err164 = *on
143700020124  "  c                   exsr      endpgm
143800020124  "  c                   endif
143900020124c1529c                   endif
144000000426C1357X*
144100000426  "  X* Se registro acquisti stampo anche indetraibili
144200000426  "  X*****tpr164        ifeq      '1'
144300000426  "  X*****              setoff                                       90
144400000426  "  X*****              else
144500000426  "  X*****              seton                                        90
144600000426C1357X*****              endif
144700951201     C*
144800951201     C* Se non passato libro non ne stampo l'intestazione
144900951201     C     lib164        ifeq      *blank
145000000426C1357X*****              seton                                        85
145100000420C1357C                   seton                                        86
145200951201     C                   else
145300000426C1357X*****              setoff                                       85
145400000420C1357C                   setoff                                       86
145500951201     C                   endif
145600950712     C*
145700950712     C* Azzero totali aliquota
145800950712     C                   z-add     0             q1imponnor
145900950712     C                   z-add     0             q1imposnor
146000950712     C                   z-add     0             q1imponind
146100950712     C                   z-add     0             q1imposind
146200950712     C                   z-add     0             q1totimpon
146300950712     C                   z-add     0             q1totimpos
146400950712     C                   z-add     0             q1totgen
146500950712     C                   z-add     0             t1totimpon
146600950712     C                   z-add     0             t1totimpos
146700950712     C                   z-add     0             t1totgen
146800951215     C                   z-add     0             t1imponnor
146900951215     C                   z-add     0             t1imposnor
147000951215     C                   z-add     0             t1imponind
147100951215     C                   z-add     0             t1imposind
147200950712     C*
147300971212     C* Azzero totali riferimento iva
147400950712     C                   z-add     0             p2imponib
147500950712     C                   z-add     0             t2totimpon
147600950712     C*
147700950712     C* Spengo indicatori lavoro
147800950712     C                   setoff                                       6061
147900950712     C                   setoff                                       7071
148000950712     C*
148100950606     C                   ENDSR
148200950606     C/EJECT
148300941103      ************************************************************
148400941103      * Reperimento dati società
148500941103      ************************************************************
148600941103     C     REPSOC        BEGSR
148700941103      *
148800950630     C                   CALLB     'XSOC'
148900941103     C                   PARM                    TIPXSC            6
149000941103     C                   PARM                    SOCXSC            3
149100941103     C                   PARM                    CDSXSC            9 0
149200941103     C                   PARM                    MODXSC            3
149300941103     C                   PARM      *BLANKS       RTNXSC            1
149400941103     C                   PARM                    XSOCDS
149500941103     C                   PARM                    KPJBA
149600941103      *
149700941103     C                   ENDSR
149800950606     C/EJECT
149900130620     C************************************************************
150000130620     C* Annullamento riga
150100130620     C************************************************************
150200130620     C     YAnnRiga      BEGSR
150300130620      *-----------------------
150400130620     C                   EVAL      NRigAc = *ALL'*'
150500130620     C                   EVAL      %SUBST(NRIGAC
150600130620     C                             :89:19)
150700130620     C                             = Annullato
150800130620
150900130620     C                   EVAL      NRigVe = *ALL'*'
151000130620     C                   EVAL      %SUBST(NRIGVE
151100130620     C                             :89:19)
151200130620     C                             = Annullato
151300130620      *
151400130620     C                   IF        Tpr164 = '1'
151500130620     C                   DOW       CurLineA < 66
151600130620     C                   WRITE     YAnnAcq
151700130620     C                   ENDDO
151800130620     C                   ENDIF
151900130620      *
152000130620     C                   IF        Tpr164 = '2'
152100130620     C                   DOW       CurLineV < 66
152200130620     C                   WRITE     YAnnVen
152300130620     C                   ENDDO
152400130620     C                   ENDIF
152500130620      *
152600130620     C                   ENDSR
