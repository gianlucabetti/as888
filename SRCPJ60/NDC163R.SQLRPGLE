000100161129>>>>>H DFTACTGRP(*NO) ACTGRP('QILE')
000200161129>>>>>H BNDDIR('PJXBND':'PJCBND':'NDC163BND':'YCO')
000400950608     H DECEDIT('0,') DATEDIT(*DMY.)
000500990427     F*-------------
000600970409     F*                      STAMPA LIBRI IVA
000700971020     F*                    --------------------
000800971020     F*-------------
000900020326C1550X* Soc/TpReg/Libro/DtReg/NrReg/DtDoc/NrDoc
001000020326C1550X*NDIVA08J  IF   E           K DISK
001100020326C1550 * Soc/TpReg/Libro/DtAnnot/NrReg/DtDoc/NrDoc
001200020402C1550FNDIVA11J  IF   E           K DISK
001300950710    >FNDMOV01L  IF   E           K DISK
001400950710    >FANOPE01L  IF   E           K DISK
001500961202    >FANALR02L  IF   E           K DISK
001600990527    >FNDIVA01L  UF   E           K DISK    PREFIX(IV1:3)
001700980821    >FANDLS02L  IF   E           K DISK
001800950706    >FNDIVAA198 O    E             PRINTER OFLIND(*IN39) USROPN
001900130620     F                                     INFDS(YInfNDIvaA)
002000950710    >FNDIVAV198 O    E             PRINTER OFLIND(*IN40) USROPN
002100130620     F                                     INFDS(YInfNDIvaV)
002200950710    >FNDIVAC198 O    E             PRINTER OFLIND(*IN41) USROPN
002300130620     D*-------------
002400130620     D YInfNDIvaA      DS
002401161129>>>>>D  fileNameA             83     92A                                        File name
002403161129>>>>>D  spoolNumA            160    163I 0                                      6 digit Spool Nbr
002500130620     D  CurLineA             367    368I 0
002501171117     D  curPageA             369    372I 0                                      Current page cnt
002600130620     D*-------------
002700130620     D YInfNDIvaV      DS
002701161129>>>>>D  fileNameV             83     92A                                        File name
002703161129>>>>>D  spoolNumV            160    163I 0                                      6 digit Spool Nbr
002800130620     D  CurLineV             367    368I 0
002801171117     D  curPageV             369    372I 0                                      Current page cnt
002900950627     D*-------------
003000900731     D KPJBA         E DS
003100950605     D*-------------
003200950130     D* Parametri del PGM
003300950703     D C163DS        E DS                  EXTNAME(NDC163DS)
003400950628     D*-------------
003500950704     D* Parametri del PGM stampa riepiloghi
003600950704     D C164DS        E DS                  EXTNAME(NDC164DS)
003700980126     D*-------------
003800000511C1357D* Parametri del pgm NDC263R (stampa corrispettivi lordi ventilazione)
003900000511  "  D NDC263DS      E DS                  INZ
004000000511C1357D*-------------
004100011212c1529D* Parametri memorizzazione intestazione
004200011212  "  D xpaout          DS          2000
004300011212  "  D* DS esterna per stampa intestazione bollato
004400011212C1529D ndc099ds      E DS
004500030418     D*-------------
004600030418C1651D* Parametro modulo IVA
004700030418  "  D ANP045DS      E DS
004800030418c1651D*-------------
004900980126     D* Richiamo a XATB
005000980126     D XATBDS        E DS                  INZ
005100980126     D*-------------
005200970725    >D* Tabella per ovrprtf
005300970725    >D AngG75Ds      E DS
005400011214     D*-------------
005500011214c1529D* Tabella numeri pagina
005600011214c1529D Ang008Ds      E DS
005700970725     D*--------
005800970725     D* DS Esterna gestione messaggi
005900970725     D XMSGDS        E DS
006000970725     D  STS          E                     EXTFLD(MSGSTS)
006100970725     D                                     DIM(333)
006200970725     D  JBA          E                     EXTFLD(MSGJBA)
006300970725     D                                     DIM(502)
006400970725     D*--------
006500970725    >D* DS per chiave tabella G75
006600970725     D DSKEY           DS
006700970725     D CdStampa                1      3
006800970725     D keyGest                 4     15
006900970725     D*--------
007000980126     D* DS esterna per reperimento decodifica dello stato
007100980126     D AngG12Ds      E DS                  INZ
007200950704     D*-------------
007300941103     D* Dati per pgm XSOC
007400941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
007500980821     D    XscIvaRgm                   1    overlay(XscFgo:9)                    Scadenz. retroattivo
007600980821     D    XscIvaLiq                   1    overlay(XscFgo:10)                   Scadenz. retroattivo
007700000511C1357D    XScIvaVent                  1A   Overlay(XscFgo:4)
007800941103     D*-------------
007900950605     D* DS Interna per dati utente
008000941103     D XSOCDS          DS          1000
008100950705     D*-------------
008200950705     D* Reperimento nome PGM
008300950705     D STATUS         SDS           333
008400950705     D  DSPGM            *PROC
008401161129>>>>>D  psJobName            244    253                                         Nome lavoro
008402161129>>>>>D  psJobUser            254    263                                         Utente
008403161129>>>>>D  psJobNbr             264    269                                         Numero lavoro
008500950706     D*-------------
008600950711     D* DS esterna per decodifica conti
008700950710     D ND002DS       E DS
008800950710     D*-------------
008900950711     D* DS esterna per dati soggetti
009000950711     D DVASOG        E DS
009100950711     D*-------------
009200950711     D* Ds per output variazioni estremi fiscali
009300950711     D ANCPI         E DS                  EXTNAME(ANCPI00F)
009400970409     D*-------------
009500990304     D* Periodo liquidazione
009600990304     D X62IVALQDS    E DS                  INZ
009700990304     D*-------------
009800970725    >D* DS per chiave tabella G75 (ultimi 12 byte della chiave di 15)
009900970725     D KeyGestDs       DS
010000970409     D StRegistro              1      1
010100970409     D Bollato                 2      2
010200970409     D TpRegistro              3      3
010300970725     D Libro                   4      6
010400950710     D*-------------
010500950711     D* Definizioni parametri decodifica conto
010600980126     D $esito          S              1
010700980126     D $gesmsg         S              1
010800980126     D $lenout         S              9B 0
010900980126     D $struttura      S             10
011000980126     D $date           S               D
011100980126     D $kcc            S              6
011200980126     D $ksc            S              8
011300980126     D $soc            S              3
011400950711     D*-------------
011500950711     D* Definizioni parametri driver estremi fiscali
011600980126     D dvcRic          S              1
011700980126     D dvcSocieta      S              3
011800980126     D dvcUnita        S              8
011900980126     D dvcStrutt       S             10
012000980126     D dvcDtRif        S               D
012100980126     D dvcLenOut       S              9B 0
012200980126     D dvcSogg         S              8
012300980126     D dvcErrore       S              1
012400980126     D dvcOutput       S           4000
012500950711     D*-------------
012600950711     D* Definizioni parametri driver soggetti
012700980126     D dvaRic          S              1
012800980126     D dvaSocieta      S              3
012900980126     D dvaUnita        S              8
013000980126     D dvaDtRif        S               D
013100980126     D dvaStrutt       S             10
013200980126     D dvaLenOut       S              9B 0
013300980126     D dvaSnatura      S              1
013400980126     D dvaCodice       S              8
013500980126     D dvaLineav       S              3
013600980126     D dvaFiliale      S              3  0
013700980126     D dvaSogg         S              8
013800980126     D dvaTpInd        S              2
013900980126     D dvaCdInd        S              3
014000980126     D dvaOutput       S           4000
014100980126     D dvaErrore       S              1
014200960711     D*-------------
014300981218     D* Definizioni parametri driver estensione nominativi partite
014400981218     D Sys             S                   Like(IVASys)
014500981218     D NrAsInt         S                   Like(IVANrAsReg)
014600981218     D NrAsGen         S                   Like(IVANrAsReg)
014700981218     D LenOut          S              9B 0
014800981218     D Esito           S              1
014900981218     D NDCPADS       E DS                  ExtName(NDCPA00F) Inz
015000981218     D*-------------
015100970729     D* definisco campi di lavoro
015200960711     D $primariga      S              1A
015300130620     D $movivaok       S              1A
015400130620     D Annullato       S             19    INZ(' A N N U L L A T O ')
015500970729     D $X              S              9S 0
015600980803     D DatMin          C                   CONST('0001-01-01')
015700980803     D DatMax          C                   CONST('9999-12-31')
015800980803     D WdatMin         S             10D   DATFMT(*ISO)
015900980803     D WIvaLiq         s              1  0
016000980803     D WDtRif          s             10D
016100990304     D PerDtLiq        S                   Like(X62PerLiq)
016200020402C1550X*PerDtReg        S                   Like(X62PerLiq)
016300020402C1550D PerDtAnnot      S                   Like(X62PerLiq)
016400030422c1651D SavNrReg8       S                   Like(NrReg008)
016500030422c1651D SavDtReg8       S                   Like(DtReg008)
016600991104B0001D*-------------
016700000222  "  D* ds salvataggio KPJBA
016800991104  "  D KPJBASav        S                   Like(KPJBA)
016900991104  "  D*-------------
017000000222  "  D* Costante con identificativo messaggio
017100991104  "  D $MSGID          C                   CONST('PROMSG    *LIBL-
017200991104B0001D                                          COS0308')
017300991104     D*-------------
017400000118C1309D ZDateISO        S               D
017500000222     D*-------------
017600130620     D ynimf00f      E DS                  INZ
017700130620     D ytpreg          S                   LIKE(imftpreg)
017800130620     D ydivisa         S                   LIKE(imfdivisa)
017900130620     D ydtval          S                   LIKE(imfdtvalfi)
018000130620     D yimpmin         S                   LIKE(imfimpmin)
018100130620     D yesito          S              1
018200130620     D yFatImpMin      S              1
018300130620     D yopzione        S              1
018400130620     D ysys            S                   LIKE(ivasys)
018500130620     D ynrasreg        S                   LIKE(ivanrasreg)
018600130620     D ysocieta        S                   LIKE(ivasocieta)
018700130620     D ylibro          S                   LIKE(ivalibro)
018800130620>>>>>D yOpCode         S             10A
018900130620>>>>>D yRtnCode        S             10A
019000130620>>>>>D yImponibile     S             15P 2
019100130620>>>>>D yImposta        S             15P 2
019200130620>>>>>D yIndOvrFlw      S              1N
019300130620
019400130620>>>>>D YCO1636R        PR
019500130620>>>>>D                                     LIKE(IvaImponib)
019600130620>>>>>D  PrSys
019700130620>>>>>D                                     LIKE(IvaSys)
019800130620>>>>>D                                     CONST
019900130620>>>>>D  PrNrAsReg
020000130620>>>>>D                                     LIKE(IvaNrAsReg)
020100130620>>>>>D                                     CONST
020200130620>>>>>D  PrImpTot
020300130620>>>>>D                                     LIKE(IvaImponib)
020400130620>>>>>D                                     CONST
020500130620>>>>>D  PrImpTotD
020600130620>>>>>D                                     LIKE(IvaImpond)
020700130620>>>>>D                                     CONST
020701170728
020702161129>>>>> /COPY GAITRASRC/SRCPROTOPR,TRBMAF05R
020703170727>>>>> /COPY GAITRASRC/SRCPROTOPR,QP0ZTRC
020704171120>>>>> /COPY GAITRASRC/SRCPROTOPR,QUSRSPLA
020705171120>>>>> /COPY QSYSINC/QRPGLESRC,QUSRSPLA
020706171120>>>>> /COPY QSYSINC/QRPGLESRC,QUSEC
020707170727>>>>> /COPY GAITRASRC/SRCCONST,TRBMAF05R
020801170728     D LINEFEED        C                   X'25'
020802170728
020900950405     C*----------------------------------------------------*
021000950405     C*                MAIN LINE PROGRAM
021100950405     C*----------------------------------------------------*
021200980126     C*
021300950606     C* Inizializzazione variabili
021400950606     C                   EXSR      INZVAR
021500950706     C*
021600950706     C* Gestione apertura PRTF in base al registro
021700950706     C                   EXSR      OPNPRT
021800990427     C*
021900990427     C* Il libro di "pura liquidazione" non viene stampato se bollato
022000990427     C                   if        sos163 <> '5' or
022100990427     C                             (sos163 = '5' and bol163 <> *on)
022200990309     C* Stampa libro IVA
022300990309     C* (se non richiesto solo totali ...
022400030513c1651c* deve entrare sempre dentro a questa routine, perchè deve
022500030513  "  c* valorizzare il campo numero protocollo (p1nrreg), d'altronde
022600030513  "  c* i test sul regime iva ci sono anche dentro alla routine
022700030513  "  c* e la stampa viene fatta solo per N85(IMPOSTATO DA TOT163 =*ON)
022800030513  "  x****               if        tot163 = *off
022900030513  "  X* ... oppure regime iva trasportatori e richiesto aggiornamento
023000030513  "  X* perchè si deve poter "slittare" la data liquidazione iva)
023100030513C1651X****                         or (XscIvaRgm = '1' and agr163 = *on)
023200950704     C                   EXSR      LIBIVA
023300030513C1651X****               endif
023400130620     C* Stampa riepilogo DPR 1996/695/6/1.
023500130620     C                   EXSR      yStpRie695
023600130620     C                   EXSR      YAnnRiga
023700950704     C* Stampa riepiloghi libro IVA
023800950704     C                   EXSR      RIEIVA
023801161130     C
023808170727>>>>>C                   IF        SendToDocFlow() < *ZERO
023809170727>>>>>C                   EVAL      err163 = *ON
023810170727>>>>>C                   EXSR      EndPgm
023811170727>>>>>C                   ENDIF
023900971211     C*
024000990427     C                   endif
024100990427     C*
024200001002     C* Aggiornamento file riepiloghi IVA solo se richiesto
024300001002C1425C* (anche per libri di iva sospesa o iva differita)
024400001002  "  X****** no iva in sospensione o differita.
024500001002  "  C                   if        agr163 = *on
024600001002  "  X*****              if        agr163 = *on and
024700001002  "  X*****                        (sos163 = '0' or sos163 = '3' or
024800001002  "  X*****                         sos163 = '5')
024900001002  "  X*****              CALL      'NDC163R1'
025000001003  "  X*****              PARM                    KPJBA
025100001003  "  C                   CallB     'NDC163R1'
025200001003C1425C                   Parm                    C163DS
025300971211     C                   endif
025400000511C1357C*
025500000511  "  C* Se la società gestisce la ventilazione ...
025600000511  "  C                   If        XScIvaVent = '1'
025700000511  "  C* ... e registro corrispettivi ...
025800000511  "  C                             and TpR163 = '3'
025900000511  "  C* Stampa libro corrispettivi lordi ventilazione
026000000511  "  C                   ExSr      StpCorLorV
026100000511C1357C                   EndIf
026200900731     C*
026300950606     C* Fine programma
026400950606     C                   EXSR      ENDPGM
026500950606     C*
026600950606     C/EJECT
026700950704     C************************************************************
026800950705     C* LIBRO IVA
026900950704     C************************************************************
027000950704     C     LIBIVA        BEGSR
027100950705     C*
027200950705     C                   move      soc163        ivasocieta
027300950705     C                   move      tpr163        ivatpreg
027400950705     C                   move      lib163        ivalibro
027500020326C1550X****               move      dti163        ivadtreg
027600020326C1550C                   move      dti163        IVADtAnnot
027700020326C1550X**** k4iva8        setll     ndiva08j
027800020402C1550C     K04IVA11      setll     NDIVA11J
027900000222     C                   do        *hival
028000020326C1550X**** k3iva8        reade     ndiva08j                               21
028100020402C1550C     K03IVA11      reade     NDIVA11J                               21
028200000222     C* Verifica se movimento definitivo e di contabilità 'CG'
028300000222     C                   EXSR      MOVDEF
028400000222     C  N22              leave
028500000222     C   21              leave
028600000222     C                   enddo
028700000222     C*
028800000222     C* Salvataggio nr./data reg./doc., data liquidaz. e status movimento
028900000222     C                   z-add     ivanrreg      savnrreg
029000020402C1550X****************** move      ivadtreg      savdtreg
029100020402C1550C                   move      IVADtAnnot    SAVDtAnnot
029200000222     C                   z-add     ivanrdoc      savnrdoc
029300030218c1615C                   move      ivaSerDoc     savSerDoc
029400000222     C                   move      ivadtdoc      savdtdoc
029500000222     C                   move      ivadtrif      savdtrif
029600000222     C                   move      ivasosp       savsosp
029700950705     C*
029800950705     C     *in21         doweq     *off
029900020327C1550X**** ivadtreg      andle     dtf163
030000020327C1550C     IVADtAnnot    andle     dtf163
030100950724     C*
030200980609     C* Nuovo documento
030300980609     C     ivanrreg      ifne      savnrreg
030400020327C1550X**** ivadtreg      orne      savdtreg
030500020327C1550C     IVADtAnnot    orne      SAVDtAnnot
030600980609     C*
030700980609     C* Stampa totale documento
030800980609     C                   EXSR      TOTDOC
030900980609     C*
031000990128     C* Salvataggio nr./data reg./doc., data liquidaz. e status movimento
031100980821     C                   z-add     ivanrreg      savnrreg
031200020402C1550X****************** move      ivadtreg      savdtreg
031300020402C1550C                   move      IVADtAnnot    SAVDtAnnot
031400990128     C                   z-add     ivanrdoc      savnrdoc
031500030218c1615C                   move      ivaSerDoc     savSerDoc
031600990128     C                   move      ivadtdoc      savdtdoc
031700980825     C                   move      ivadtrif      savdtrif
031800980821     C                   move      ivasosp       savsosp
031900980821     C*
032000980821     C                   endif
032100980821     C*
032200980821     C     *in60         ifeq      *off
032300010905C1514 **********************************************************
032400010906  "   * Accendo l'ind 60 solo dopo che ho stampato l'intestaz. *
032500010906  "   * della prima pagina, così dopo posso stampare anche     *
032600010906  "   * all'inizio di ogni pagina, il tot imponibile e tot iva *
032700010906  "   * della pagina precedente                                *
032800010905C1514 **********************************************************
032900010905C1514X**Accendo indicatore "stampa registrazione"
033000010905C1514X****************** seton                                        60
033100980821     C* Stampa testata
033200980821     C                   EXSR      TESTA
033300010905C1514C* Accendo indicatore "stampata registrazione"
033400010905C1514C                   seton                                        60
033500980821     C                   endif
033600980821     C*
033700990427     C* Ricalcolo data liquidazione iva solo se:
033800000222     C* - Il regime iva è "trasportatori" (XscIvaRgm='1')
033900000222     C* - La causale di testata ha lo slittamento del libro (OpeDecala='2')
034000000222     C* - E' stato scelto l'aggiornamento del riepilogo Iva (Agr163='1')
034100990427     C* - Lo slittamento ha termini temporali validi                        .
034200020530C1550X*** (DlsDtInVal <= IvaDtreg =< DlsDtFival)                            .
034300020530C1550C*   (DlsDtInVal <= IvaDtAnnot =< DlsDtFival)                          .
034400990427   B1C                   If        XscIvaRgm='1' and
034500990427     C                             Agr163=*on
034600020530C1550 * i movimenti da stampare sono stati selezionati per dt annot.
034700020530  "   * quindi per congruenza, anche il criterio di calcolo slittamento
034800020530C1550 * deve considerare data annotazione e non data reg
034900020530C1550X****************** move      ivadtreg      wdtrif
035000020530C1550C                   move      IVADtAnnot    wdtrif
035100990427     C                   move      soc163        dlssocieta
035200990427     C                   move      tpr163        dlstpreg
035300990427     C                   move      lib163        dlslibro
035400020530C1550X****************** move      ivadtreg      dlsdtfival
035500020530C1550C                   move      IVADtAnnot    dlsdtfival
035600990427     C     K4Dls2        Setll     AnDls000
035700990427     C     K3Dls2        Reade     AnDls000                               22
035800990427   B2C                   If        *In22=*Off and
035900020530C1550X******************           IvaDtReg >= DlsDtInval  and
036000020530C1550X******************           IvaDtReg <= DlsDtFiVal
036100020530C1550C                             IVADtAnnot >= DlsDtInval  and
036200020530C1550C                             IvaDtAnnot <= DlsDtFiVal
036300990427     C                   move      dlsslitipo    slitipo           1
036400990427     C                   move      dlsslitemp    slitemp           3 0
036500990427     C                   move      soc163        OpeSocieta
036600990427     C                   move      ivacaustes    Opecausale
036700990427     C     K2Ope1        chain     Anope000                           22
036800990427   B3C                   If        *In22=*Off    and
036900990427     C                             OpeDecala='2'
037000990427     C* Ricalcolo data riferimento IVA
037100990427     C                   Exsr      CalDtr
037200990427   E3C                   EndIf
037300990427   E2C                   EndIf
037400000222     C* Aggiorno ndiva01l con data rif. ricalcolata o con data reg.
037500130620>>>>>** Non aggiorno la data liquidazione perchè l'ho già aggiornata
037600130620>>>>>** prima di eseguire la stampa del libro.
037700130620>>>>>C*                  Exsr      AggIva
037800990427   E1C                   EndIf
037900960418     C*
038000950719     C* Segno
038100950719     C                   select
038200950719     C     ivatpreg      wheneq    '1'
038300950719     C     ivaavere      andeq     1
038400950719     C     ivaimporto    mult      -1            ivaimporto
038500950719     C     ivaimponib    mult      -1            ivaimponib
038600950719     C     ivaimpond     mult      -1            ivaimpond
038700950719     C     ivatpreg      wheneq    '2'
038800950719     C     ivadare       andeq     1
038900950719     C     ivatpreg      oreq      '3'
039000950719     C     ivadare       andeq     1
039100950719     C     ivaimporto    mult      -1            ivaimporto
039200950719     C     ivaimponib    mult      -1            ivaimponib
039300950719     C     ivaimpond     mult      -1            ivaimpond
039400950719     C                   endsl
039500990427     C*
039600950710     C* Stampa riga libro
039700950705     C                   EXSR      STPLIB
039800960418     C*
039900000222     C                   do        *hival
040000020326C1550X**** k3iva8        reade     ndiva08j                               21
040100020402C1550C     K03IVA11      reade     NDIVA11J                               21
040200000222     C* Verifica se movimento definitivo e di contabilità 'CG'
040300000222     C                   EXSR      MOVDEF
040400000222     C  N22              leave
040500000222     C   21              leave
040600000222     C                   enddo
040700950705     C*
040800950705     C                   enddo
040900950712     C*
041000950712     C* Stampa ultimo totale documento
041100950712     C     *in60         ifeq      *on
041200950712     C                   EXSR      TOTDOC
041300010905C1514 *
041400010905  "   *  stampo il totale pagina a fine pagina
041500010905  "   *    visualizzo 'TOTALE PROGRESSIVO:' (73N74)
041600010905  "  C                   eval      *In74 = *Off
041700010905  "  C                   eval      *In73 = *On
041800010905  "  C                   exsr      StpTotPag
041900010905C1514 *
042000950712     C                   endif
042100950705     C*
042200950704     C                   ENDSR
042300950704     C/EJECT
042400000222     C************************************************************
042500000222     C* CONTROLLA MOVIMENTO DEFINITIVO E DI CONTABILITA' CG
042600000222     C************************************************************
042700000222     C     MOVDEF        BEGSR
042800000222     C*
042900000222     C                   setoff                                       22
043000000222     C*
043100000222     C                   move      ivasys        movsys
043200000222     C                   move      ivanrasreg    movnrasreg
043300000222     C                   move      ivanrrigam    movnrrigam
043400000222     C     k3mov1        chain     ndmov01l                           22
043500000222     C                   if        *in22 = *off and
043600000222     C                             (movtpregz <> 'D' or movctb <> ctb163)
043700000222     C                   seton                                        22
043800000222     C                   endif
043900000222     C*
044000000222     C                   ENDSR
044100000222     C/EJECT
044200980803     C************************************************************
044300980803     C* CALCOLO DATA RIFERIMENTO IVA
044400980803     C************************************************************
044500980803     C     CALDTR        BEGSR
044600990427     C*
044700980821     C                   Move      XscIvaLiq     WIvaLiq
044800990427     C*
044900990427     C                   Select
045000990427     C*
045100990427     C* Liq. Successiva
045200990427     C     SliTipo       Wheneq    '1'
045300990427     C                   If        WIvaLiq=0
045400990427     C                   adddur    1:*y          wdtrif
045500990427     C                   Else
045600990427     C                   adddur    WIvaLiq:*m    wdtrif
045700990427     C                   EndIf
045800990427     C*
045900990427     C* slittamento in giorni
046000990427     C     SliTipo       Wheneq    '2'
046100990427     C                   adddur    Slitemp:*d    wdtrif
046200990427     C*
046300990427     C* slittamento in mesi
046400990427     C     SliTipo       Wheneq    '3'
046500990427     C                   adddur    Slitemp:*m    wdtrif
046600990427     C*
046700990427     C                   EndSl
046800980803     C*
046900980803     C                   ENDSR
047000980803     C/EJECT
047100980803     C************************************************************
047200980803     C* AGGIORNAMENTO FILE NDIVA - MOVIMENTI IVA
047300980803     C************************************************************
047400980803     C     AGGIVA        BEGSR
047500980803     C*
047600990427     C* Salvo data di riferimento
047700990427     C                   move      wdtrif        savdtrif
047800990427     C*
047900990527     C                   move      ivasys        iv1sys
048000990527     C                   move      ivanrasreg    iv1nrasreg
048100990527     C                   move      ivanrrigam    iv1nrrigam
048200990527     C                   move      ivanrriga     iv1nrriga
048300980803     C*
048400980803     C     k4iva1        chain     ndiva01l                           22
048500980803     C*
048600980803     C     *in22         ifeq      *off
048700990527     C                   move      wdtrif        iv1dtrif
048800980803     C                   update    ndiva000
048900980803     C                   endif
049000980803     C*
049100980803     C                   ENDSR
049200980803     C/EJECT
049300950705     C************************************************************
049400950705     C* STAMPA LIBRO IVA
049500950705     C************************************************************
049600950705     C     STPLIB        BEGSR
049700950705     C*
049800960711     C* Reperimento dati anagrafici (solo per la prima riga del documento)
049900960711     C     $primariga    ifne      *on
050000960711     C                   EXSR      DATANA
050100130620>>>>>C                   setoff                                       70
050200960711     C                   endif
050300990527     C* registrazione
050400950705     C                   z-add     ivanrreg      p1nrreg
050500020327C1550X**** ivadtreg      ifeq      *loval
050600020327C1550C     IVADtAnnot    ifeq      *loval
050700971211     C                   z-add     0             p1dtreg
050800950705     C                   else
050900000118C1309C******dmy          move      ivadtreg      p1dtreg
051000020327C1550X******JobRun       move      ivadtreg      p1dtreg
051100020327C1550C     *JobRun       move      IVADtAnnot    p1dtreg
051200950705     C                   endif
051300990527     C* causale testata
051400990527     C                   move      ivacaustes    p1caustes
051500950706     C                   move      soc163        opesocieta
051600990527     C                   move      ivacaustes    opecausale
051700950706     C     k2ope1        chain     anope01l                           22
051800990527     C  N22              movel     opedesbrev    p1descaust
051900990527     C   22              move      *blank        p1descaust
052000130620     C* Controllo importo minimo fattura vendita.
052100130620     C                   EXSR      yChkImpMin
052200990527     C* causale riga
052300990527     C                   move      ivacausrig    p1causrig
052400990527     C* conto
052500950705     C                   move      ivaksccp      p1ksccp
052600990527     C* imponibile m.c.
052700950706     C                   z-add     ivaimponib    p1imponib
052800990527     C* aliquota e imposta m.c.
052900950710     C     ivaaliq       ifne      0
053000950710     C                   setoff                                       80
053100950710     C                   z-add     ivaimporto    p1importo
053200950706     C                   z-add     ivaaliq       p1aliq
053300990527     C* riferimento IVA
053400950710     C                   else
053500950710     C                   seton                                        80
053600961202     C                   move      ivasocieta    alrsocieta
053700961202     C                   move      ivatpreg      alrtpreg
053800961202     C                   move      ivalibro      alrlibro
053900961202     C                   z-add     0             alraliq
054000961202     C                   move      ivarifiva     alrrifiva
054100961202     C     k5alr1        chain     analr02l                           22
054200961202     C  N22              move      alrdesbrev    p1desrif
054300950710     C   22              move      *blank        p1desrif
054400950710     C                   endif
054500950710     C*
054600951218     C* IVA indetraibile
054700951218     C     ivaindetr     ifeq      *on
054800951218     C                   seton                                        75
054900951218     C                   else
055000951218     C                   setoff                                       75
055100951218     C                   endif
055200960418     C*
055300950710     C* Verifica moneta di conto / divisa registrazione
055400960418     C     movdivisa     ifne      xscdiv
055500950710     C                   z-add     ivaimpond     p1impond
055600990128     C                   z-add     ivaimportd    p1importd
055700950710     C                   move      movdivisa     p1divisa
055800130620>>>>>C                   seton                                        01
055900950710     C                   else
056000950710     C                   z-add     0             p1impond
056100990128     C                   z-add     0             p1importd
056200950710     C                   move      *blank        p1divisa
056300130620>>>>>C                   setoff                                       01
056400950710     C                   endif
056500130620>>>>>C*
056600130620>>>>>C                   setoff                                         7172
056700130620>>>>>C*
056800130620>>>>>C* Se la data liquidazione è di un periodo (di liquidazione) diverso
056900130620>>>>>C*  da quello della data registrazione --> la stampo
057000130620>>>>>C**   ivadtrif      ifne      ivadtreg
057100130620>>>>>C**                 reset                   x62ivalqds
057200130620>>>>>C**                 move      soc163        x62societa
057300130620>>>>>C**                 extrct    ivadtrif:*y   x62anno
057400130620>>>>>C**                 extrct    ivadtrif:*m   x62mese
057500130620>>>>>C**                 call      'X62IVALQ'
057600130620>>>>>C**                 parm                    x62ivalqds
057700130620>>>B3C**   x62esito      ifeq      '0'
057800130620>>>>>C**                 move      x62perliq     perdtliq
057900130620>>>E3C**                 endif
058000130620>>>>>C**                 reset                   x62ivalqds
058100130620>>>>>C**                 move      soc163        x62societa
058200130620>>>>>C**                 extrct    ivadtreg:*y   x62anno
058300130620>>>>>C**                 extrct    ivadtreg:*m   x62mese
058400130620>>>>>C**                 call      'X62IVALQ'
058500130620>>>>>C**                 parm                    x62ivalqds
058600130620>>>B3C**   x62esito      ifeq      '0'
058700130620>>>>>C**                 move      x62perliq     perdtreg
058800130620>>>E3C**                 endif
058900130620>>>>>C**                 if        perdtliq <> perdtreg
059000130620>>>>>C                   IF        IvaTpReg = '2'
059100130620     C                   seton                                        71
059200130620>>>>>C                   ENDIF
059300130620>>>>>C     *dmy          move      ivadtannot    p1dtrif
059400130620>>>>>C**                 endif
059500130620>>>>>C**                 endif
059600130620>>>>>C*
059700130620>>>>>C* Documento
059800130620>>>>>C                   z-add     ivanrdoc      p1nrdoc
059900130620>>>>>C     ivadtdoc      ifeq      *loval
060000130620>>>>>C                   z-add     0             p1dtdoc
060100130620>>>>>C                   else
060200130620>>>>>C     *dmy          move      ivadtdoc      p1dtdoc
060300130620>>>>>C                   endif
060400130620      * Stampo la riga IVA
060500130620      * se non è una fattura sotto l'importo minimo
060600130620      * oppure se non è stato chiesto il bollato.
060700130620     C                   IF        yFatImpMin = *OFF
060800130620     C                             OR
060900130620     C                             Bol163 = *OFF
061000010905     C* Stampa riga libro
061100950705     C     *in39         ifeq      *on
061200950710     C     *in40         oreq      *on
061300950710     C     *in41         oreq      *on
061400950710     C                   EXSR      TESTA
061500950705     C                   endif
061600950711     C*
061700010905C1514 *
061800010905  "   * totalizzo imponibile e iva m.c per stamparlo a fine pagina
061900010905  "   * e all'inizio della pagina seccessiva
062000010905  "  C                   add       P1Imponib     TPImponib
062100010905  "   * totalizzo iva m.c solo se c'è l'aliquota
062200010905  "  C  N80              add       P1Importo     TPImporto
062300010905C1514 *
062400950710     C                   select
062500950710     C     tpr163        wheneq    '1'
062600990311     C  N85              WRITE     RIGP1A
062700950711     C     tpr163        wheneq    '2'
062800990311     C  N85              WRITE     RIGP1V
062900950711     C     tpr163        wheneq    '3'
063000990311     C  N85              WRITE     RIGP1C
063100950711     C* Decodifica corrispettivi (2a riga)
063200971212     C                   if        ivcdescr2 <> *blank
063300950711     C     *in39         ifeq      *on
063400950711     C     *in40         oreq      *on
063500950711     C     *in41         oreq      *on
063600950711     C                   EXSR      TESTA
063700950711     C                   endif
063800950711     C                   movel     ivcdescr2     p2descr
063900990311     C  N85              WRITE     RIGP2C
064000971212     C                   endif
064100950711     C                   endsl
064200950710     C*
064300951128     C                   seton                                        70
064400951128     C*
064500130620     C                   ENDIF
064600130620     C*
064700950705     C                   ENDSR
064800950705     C/EJECT
064900960711     C************************************************************
065000960711     C* REPERIMENTO DATI ANAGRAFICI
065100960711     C************************************************************
065200960711     C     DATANA        BEGSR
065300960711     C*
065400960711     C                   move      *on           $primariga
065500960711     C*
065600960711     C                   movel     *blank        p1descr
065700960711     C                   movel     *blank        p1partiva
065800960711     C                   movel     *blank        p1indiriz
065900960711     C                   movel     *blank        p1cap
066000960711     C                   movel     *blank        p1localit
066100960711     C                   movel     *blank        p1prov
066200971212     C                   movel     *blank        p1stat
066300971212     C                   movel     *blank        p1stad
066400960711     C*
066500971212     C* Se registro acquisti/vendite oppure corrispettivi non valorizzato
066600971212     C* reperisco dati anagrafici
066700960711     C     tpr163        ifne      '3'
066800971212     C     ivcdescr      oreq      *blank
066900960711     C*
067000960711     C                   EXSR      CALLMVC002
067100960711     C     $esito        ifeq      *off
067200960711     C*
067300960711     C* Reperimento dati da variazioni estremi fiscali
067400960711     C                   EXSR      CALLDVACPI
067500960711     C     dvcerrore     ifeq      *off
067600960711     C                   movel     dvcoutput     ancpi
067700960711     C     cpipartiva    ifeq      *blank
067800960711     C                   movel     cpicdfisc     p1partiva
067900960711     C                   else
068000960711     C                   movel     cpipartiva    p1partiva
068100960711     C                   endif
068200960711     C                   movel     cpidessog1    p1descr
068300980126     C* reperisco stato su G12
068400980126     C                   Exsr      RepStato
068500980126     C                   if        NazG12 = *Blank
068600980126     C                   movel     *Blank        P1PartIva
068700980126     C                   endif
068800980126     C                   movel     CpiStato      P1Stat
068900980126     C                   movel     DesG12        P1StaD
069000970409     C                   movel     cpiindriz     P1Indiriz
069100970409     C                   movel     cpicap        P1Cap
069200970409     C                   movel     cpilocalit    P1Localit
069300970409     C                   movel     cpiprov       P1Prov
069400960711     C                   else
069500960711     C*
069600960711     C* Reperimento dati da anagrafica soggetti
069700960711     C                   EXSR      CALLDVASOG
069800970409     C     DvaErrore     ifeq      *off
069900970409     C                   movel     DvaOutput     dvasog
070000970409     C     DvsPartIva    ifeq      *blank
070100960711     C                   movel     dvscdfisc     p1partiva
070200960711     C                   else
070300960711     C                   movel     dvspartiva    p1partiva
070400960711     C                   endif
070500990419     C                   movel     dvsdes        p1descr
070600980126     C* reperisco stato su G12
070700980126     C                   Exsr      RepStato
070800980126     C                   if        nazg12=*blank
070900980126     C                   movel     *blank        p1partiva
071000980126     C                   endif
071100980126     C                   movel     dvsstato      p1stat
071200980126     C                   movel     desg12        p1stad
071300960711     C                   movel     dvsindriz     p1indiriz
071400960711     C                   movel     dvscap        p1cap
071500960711     C                   movel     dvslocalit    p1localit
071600960711     C                   movel     dvsprov       p1prov
071700970409     C                   endif
071800960711     C*
071900960711     C                   endif
072000981218     C*
072100981218     C* Se "conto tecnico"='2' significa che è un conto gestito con
072200981218     C*  "estensione nominativi partite", quindi li reperisco ...
072300981218     C                   If        Tecnico002 = '2'
072400981218     C                   ExSr      RepEstNomP
072500981218     C                   EndIf
072600960711     C*
072700960711     C                   endif
072800960711     C*
072900960711     C                   else
073000960711     C*
073100960711     C* Decodifica corrispettivi
073200960711     C                   movel     ivcdescr      p1descr
073300960711     C*
073400960711     C                   endif
073500960711     C*
073600960711     C                   ENDSR
073700960711     C/EJECT
073800970409     C************************************************************
073900970409     C* Reperimento stato
074000970409     C************************************************************
074100970409     C     RepStato      BEGSR
074200970409     C*
074300970409     C                   clear                   AngG12Ds
074400970409     C                   MOVE      '1'           XTBRIC
074500970409     C                   MOVE      ivasocieta    XTBAZI
074600970409     C                   MOVE      *ZERO         XTBKEY
074700970409     C                   MOVE      'G12'         XTBCOD
074800120809D2748X****               if        tpr163 <> '3' and cpistato <> *blank and
074900120809D2748X****                         dvcerrore = *off
075000120809D2748C                   if        (tpr163 <> '3' and cpistato <> *blank and
075100120809  "  C                             dvcerrore = *off) or
075200120809  "  C                             (Ivcdescr = *blank and cpistato <> *blank and
075300120809D2748C                              dvcerrore = *off)
075400970409     C                   MOVE      cpistato      XTBKEY
075500970409     C                   else
075600970409     C                   MOVE      dvsstato      XTBKEY
075700970409     C                   endif
075800011214     C                   CALLB     'XATB'
075900970409     C                   PARM                    XATBDS
076000970409   B3C     XTBERR        IFEQ      '0'
076100970409     C                   MOVEL     XTBUNI        AngG12Ds
076200971212   E2C                   ELSE
076300971212   E2C                   move      *blank        p1stat
076400971212   E2C                   move      *blank        p1stad
076500970409   E2C                   ENDIF
076600971212     C*
076700971212     C                   ENDSR
076800970409     C/EJECT
076900950710     C************************************************************
077000950710     C* STAMPA TOTALI DOCUMENTO
077100950710     C************************************************************
077200950710     C     TOTDOC        BEGSR
077300950710     C*
077400980114     C                   setoff                                       707172
077500960711     C                   move      *off          $primariga
077600950711     C*
077700990304     C* Se la data liquidazione è di un periodo (di liquidazione) diverso
077800990304     C*  da quello della data registrazione --> la stampo
077900130620>>>>>C**   savdtrif      ifne      savdtreg
078000130620>>>>>C**                 reset                   x62ivalqds
078100130620>>>>>C**                 move      soc163        x62societa
078200130620>>>>>C**                 extrct    savdtrif:*y   x62anno
078300130620>>>>>C**                 extrct    savdtrif:*m   x62mese
078400130620>>>>>C**                 call      'X62IVALQ'
078500130620>>>>>C**                 parm                    x62ivalqds
078600130620>>>B3C**   x62esito      ifeq      '0'
078700130620>>>>>C**                 move      x62perliq     perdtliq
078800130620>>>E3C**                 endif
078900130620>>>>>C**                 reset                   x62ivalqds
079000130620>>>>>C**                 move      soc163        x62societa
079100130620>>>>>C**                 extrct    savdtreg:*y   x62anno
079200130620>>>>>C**                 extrct    savdtreg:*m   x62mese
079300130620>>>>>C**                 call      'X62IVALQ'
079400130620>>>>>C**                 parm                    x62ivalqds
079500130620>>>B3C**   x62esito      ifeq      '0'
079600130620>>>>>C**                 move      x62perliq     perdtreg
079700130620>>>E3C**                 endif
079800130620>>>>>C**                 if        perdtliq <> perdtreg
079900130620>>>>>C**                 seton                                        71
080000130620>>>>>C**   *dmy          move      savdtrif      p1dtrif
080100130620>>>>>C**                 endif
080200130620>>>>>C**                 endif
080300130620>>>>>C**
080400130620>>>>>C**Se il libro è sospeso/differito oppure lo è il singolo documento iva
080500130620>>>>>C**stampo la dicitura 'IVA DIFFERITA'
080600130620>>>>>C**                 if        sos163 = '1' or sos163 = '4' or
080700130620>>>>>C**                           savsosp = '1'
080800130620>>>>>C**                 seton                                        72
080900130620>>>>>C**                 setoff                                       71
081000130620>>>>>C**                 endif
081100130620>>>>>C**
081200130620>>>>>C**Documento
081300130620>>>>>C**                 z-add     savnrdoc      p1nrdoc
081400130620>>>>>C**   savdtdoc      ifeq      *loval
081500130620>>>>>C**                 z-add     0             p1dtdoc
081600130620>>>>>C**                 else
081700130620>>>>>C**   *dmy          move      savdtdoc      p1dtdoc
081800130620>>>>>C**                 endif
081900990527     C* documento
082000030218C1615C                   move      SavSerDoc     P1serDoc
082100030218     C                   z-add     Savnrdoc      p1nrdoc
082200990128     C     savdtdoc      ifeq      *loval
082300990128     C                   z-add     0             p1dtdoc
082400990128     C                   else
082500000118C1309C******dmy          move      savdtdoc      p1dtdoc
082600000118C1309C     *JobRun       move      savdtdoc      p1dtdoc
082700990128     C                   endif
082800980114     C*
082900980126     C* Se richiesto non stampo l'indirizzo
083000980126     C                   if        ind163 = *off
083100980126     C                   clear                   p1indiriz
083200980126     C                   clear                   p1cap
083300980126     C                   clear                   p1localit
083400980126     C                   clear                   p1prov
083500980126     C                   clear                   p1stat
083600980126     C                   clear                   p1stad
083700980126     C                   endif
083800130620>>>>>C* Stampo la riga di totale solo se richiesto indirizzo
083900130620>>>>>C                   if        ind163 = *on
084000950711     C                   select
084100950711     C     tpr163        wheneq    '1'
084200990311     C  N85              WRITE     TOTP1A
084300950711     C     tpr163        wheneq    '2'
084400990311     C  N85              WRITE     TOTP1V
084500950711     C     tpr163        wheneq    '3'
084600990311     C  N85              WRITE     TOTP1C
084700950711     C                   endsl
084800130620>>>>>C                   endif
084900950710     C*
085000950710     C                   ENDSR
085100950710     C/EJECT
085200950710     C************************************************************
085300950710     C* STAMPA TESTATA
085400950710     C************************************************************
085500950710     C     TESTA         BEGSR
085600950710     C*
085700010905C1514 *
085800010905  "   *  prima di stampare l'intestazione della pagina successiva
085900010905  "   *  stampo il totale pagina a fine pagina
086000010905  "   *    visualizzo 'TOTALE PROGRESSIVO:' (73N74)
086100010905  "  C                   eval      *In74 = *Off
086200010905  "  C                   eval      *In73 = *On
086300010905  "  C                   exsr      StpTotPag
086400010905C1514 *
086500950710     C     bol163        ifeq      *off
086600011214c1529C     int163        andeq     *off
086700950710     C                   select
086800950710     C     tpr163        wheneq    '1'
086900990311     C  N85              WRITE     TESTA1A
087000950710     C     tpr163        wheneq    '2'
087100990311     C  N85              WRITE     TESTA1V
087200950710     C     tpr163        wheneq    '3'
087300990311     C  N85              WRITE     TESTA1C
087400950710     C                   endsl
087500011214     c                   endif
087600011214     c*
087700011214     c* se richieste intestazioni(indipendentemente dal bollato)
087800011214C1529C                   if        int163 = *on
087900011212  "  c* se bollato stampo intestazione memorizzata su param. modulo
088000011212  "  c                   eval      T0riga1 =rg1099
088100030218c1529c* anno/pagina
088200030218c1615x***  anno099       ifne      *blanks
088300030218  "  x*                  eval      %subst(T0riga1:122:4) = anno099
088400030218  "  x*                  eval      %subst(T0riga1:126:1) = '/'
088500030218  "  x*                  endif
088600030218  "  x*                  move      NumeroPag     T0riga1
088700030218  "  c                   move      numeropag     numpag            6
088800030218C1615C                   eval      P1AnnoPag= Anno099+ '/' + numpag
088900030218c1529c                   eval      T0riga2 =rg2099
089000011212  "  c                   eval      T0riga3 =rg3099
089100011212  "  c                   eval      T0riga4 =rg4099
089200011212  "  c                   select
089300080704C1529C     tpr163        wheneq    '1'
089400080704D2410x*****              WRITE     Testa0A
089500080704D2410c  N85              WRITE     Testa0A
089600080704c1529C     tpr163        wheneq    '2'
089700080704D2410x*****              WRITE     Testa0V
089800080704D2410c  N85              WRITE     Testa0V
089900080704c1529C     tpr163        wheneq    '3'
090000080704D2410x*****              WRITE     Testa0C
090100080704D2410c  N85              WRITE     Testa0C
090200080704c1529c                   endsl
090300011214  "  C                   ADD       1             NumeroPag
090400011214c1529c                   endif
090500950710     C*
090600950710     C                   select
090700950710     C     tpr163        wheneq    '1'
090800990311     C  N85              WRITE     TESTA2A
090900990311     C  N85              WRITE     TESTA3A
091000950710     C     tpr163        wheneq    '2'
091100990311     C  N85              WRITE     TESTA2V
091200990311     C  N85              WRITE     TESTA3V
091300950710     C     tpr163        wheneq    '3'
091400990311     C  N85              WRITE     TESTA2C
091500990311     C  N85              WRITE     TESTA3C
091600950710     C                   endsl
091700950710     C*
091800010905C1514 *
091900010905  "   *  dopo aver stampato l'intestazione della nuova pagina
092000010905  "   *  stampo il saldo pagina precedente
092100010905  "   *    visualizzo 'SALDO PRECEDENTE:' (74N73)
092200010905  "  C                   eval      *In74 = *On
092300010905  "  C                   eval      *In73 = *Off
092400010905  "  C                   exsr      StpTotPag
092500010905C1514 *
092600950710     C                   setoff                                       39
092700950710     C                   setoff                                       40
092800950710     C                   setoff                                       41
092900950710     C*
093000950710     C                   ENDSR
093100980126     C/EJECT
093200010905C1514 ************************************************************
093300010905  "   * stampo il totale pagina a fine pagina
093400010905  "   ************************************************************
093500010905  "  C     StpTotPag     Begsr
093600010905  "   *
093700010905  "   * Solo se non è la prima pagina che stampo (60)
093800010905  "   * prima di stampare l'intestazione della pagina successiva
093900010905  "   * stampo il totale di pagina a fine pagina
094000010905  "   * (se ho stampato le righe di dettaglio (N85))
094100010905  "  C                   if        *In60 = *On
094200010905  "   *
094300010905  "  C                   select
094400010905  "   *
094500010905  "   * registro acquisti:
094600010905  "  C     TpR163        wheneq    '1'
094700010905  "  C  N85              write     TotPagA
094800010905  "   *
094900010905  "   * registro vendite:
095000010905  "  C     TpR163        wheneq    '2'
095100010905  "  C  N85              write     TotPagV
095200010905  "   *
095300010905  "   * registro corrispettivi:
095400010905  "  C     TpR163        wheneq    '3'
095500010905  "  C  N85              write     TotPagC
095600010905  "   *
095700010905  "  C                   endsl
095800010905  "   *
095900010905  "  C                   endif
096000010905  "   *
096100010905C1514C     XStpTotPa     Endsr
096200950704     C************************************************************
096300950704     C* STAMPA RIEPILOGHI LIBRO IVA
096400950704     C************************************************************
096500950704     C     RIEIVA        BEGSR
096600950704     C*
096700950704     C                   move      soc163        soc164
096800960515     C                   move      ctb163        ctb164
096900950704     C                   move      tpr163        tpr164
097000950704     C                   move      lib163        lib164
097100950707     C                   move      dbr163        dbr164
097200950704     C                   move      dti163        dti164
097300950704     C                   move      dtf163        dtf164
097400950707     C                   move      bol163        bol164
097500020124C1529C                   move      int163        int164
097600030115C1611 * operazioni attive/passive di dettaglio (richiedibili da C170)
097700030115C1611C                   move      *off          OpAPDet164
097800971212     C* Data registrazione
097900971212     C                   move      'R'           tpd164
098000971212     C* Chiusura in LR
098100951201     C                   move      'L'           end164
098200020125c1529C* passo ultimo numero di pagina
098300020125  "  c*   se richieste intestazioni, perchè deve proseguire da qui
098400020125  "  c                   if        int163= *on
098500020125  "  c                   z-add     numeropag     pag164
098600020125c1529c                   endif
098700030310D1300c* Propongo ilnumero e data che ci sono già memorizzati
098800030422c1651c                   z-add     SavNrReg8     NrReg164
098900030422  "  C                   IF        SavDtReg8<> *BLANKS
099000030422c1651c                   move      SavDtReg8     DtReg164
099100030422d1300C                   else
099200030310  "  c                   move      *loval        DtReg164
099300030310  "  c                   endif
099400030310  "  C* se numero protocollo diverso da 0 e se NON RISTAMPA
099500030310  "  c* passo al programma ndc164r ultimo numero protocollo E DATA fine
099600030310  "  c                   if        (tpr163='1' And RstA163 <> *on) OR
099700030310  "  c                             (tpr163='2' And Rstv163 <> *on) OR
099800030310  "  c                             (tpr163='3' And Rstc163 <> *on)
099900030310  "  c                   if        P1Nrreg <> 0
100000030307  "  c                   z-add     P1NrReg       NrReg164
100100030310  "  c                   move      DtF163        DtReg164
100200030310  "  C                   endif
100300030310D1300C                   endif
100400950704     C*
100500950704    >C                   MOVEL     C164DS        KPJBU
100600950704    >C                   CALL      'NDC164R'
100700950704     C                   PARM                    KPJBA
100800950704    >C                   MOVEL     KPJBU         C164DS
100900030508     C*
101000030508C1651c* ripasso ultimo numero di pagina a NDC163R
101100030508  "  C                   Z-add     pag164        pag163
101200030508C1651C*
101300950704     C                   ENDSR
101400950704     C/EJECT
101500000511C1357C************************************************************
101600000511  "  C* Stampa libro corrispettivi lordi ventilazione
101700000511C1357C************************************************************
101800000511     C     StpCorLorV    BEGSR
101900000511     C*
102000000511     C                   Clear                   NDC263DS
102100000511     C                   Eval      Societa263 = Soc163
102200000511     C                   Eval      Ctb263 = Ctb163
102300000511     C                   Eval      DataI263 = DtI163
102400000511     C                   Eval      DataF263 = DtF163
102500000511     C                   Eval      Libro263 = Lib163
102600000511     C                   Eval      Bollato263 = Bol163
102700000511     C                   MoveL     NDC263DS      KPJBU
102800000511     C*
102900000511     C                   Call      'NDC263R'
103000000511     C                   Parm                    KPJBA
103100000511     C                   MoveL     KPJBU         NDC263DS
103200000511     C*
103300000511C1357C                   ENDSR
103400950706     C************************************************************
103500950706     C* APERTURA PRTF
103600950706     C************************************************************
103700950706     C     OPNPRT        BEGSR
103800950706     C*
103900980327     C* aggancio tabella G75
104000970725     C                   Exsr      LeggiG75
104100970409     C*
104200980327     C* apro i file e continuo anche se non agganciata tab.G75 !!!
104300970409     C                   Select
104400970409     C     TpR163        WhenEq    '1'
104401170727>>>>>C                   IF        NOT %OPEN(ndIvaA198)
104500970409     C                   Open      NdIvaA198
104501171120>>>>>C                   CLEAR                   curPageA
104503170727>>>>>C                   ENDIF
104600970409     C     TpR163        WhenEq    '2'
104601170727>>>>>C                   IF        NOT %OPEN(ndIvaV198)
104700970409     C                   Open      NdIvaV198
104701171120>>>>>C                   CLEAR                   curPageV
104703170727>>>>>C                   ENDIF
104800970409     C     TpR163        WhenEq    '3'
104801170727>>>>>C                   IF        NOT %OPEN(ndIvaC198)
104900970409     C                   Open      NdIvaC198
104901171120>>>>>C*                  CLEAR                   curPageC
104902170727>>>>>C                   ENDIF
105000970409     C                   EndSl
105100950706     C*
105200950706     C                   ENDSR
105300950706     C/EJECT
105400970409     C***********************************************************
105500970725     C* Leggo G75 per reperire comando di printer file
105600970409     C***********************************************************
105700970725     C     LeggiG75      BEGSR
105800980327     C*
105900970725     C                   MOVE      '1'           XTBRIC
106000970725     C                   MOVE      Soc163        XTBAZI
106100970725     C                   MOVE      *Zero         XTBKEY
106200970725     C                   MOVE      'G75'         XTBCOD
106300970725     C                   MOVE      *Blank        XTBUNT
106400980327     C                   Clear                   KeyGestDs
106500980327     C* Stampa registro mensile
106600980327     C                   Move      '1'           StRegistro
106700970725     C* FormType (solo se bollato)
106800970725     C                   if        Bol163 = *On
106900970725     C                   move      'D'           Bollato
107000980327     C                   else
107100970725     C                   move      'P'           Bollato
107200970725     C                   endif
107300970725     C* Tipo registro da stampare
107400980327     C                   Eval      TpRegistro = TpR163
107500991108     C                   Eval      CdStampa  =   'IVA'
107600970725     C                   Eval      KeyGest   =   KeyGestDs
107700970725     C                   MOVE      DsKey         XTBKEY
107800970725     C*
107900970725     C                   CALLB     'XATB'
108000970725     C                   PARM                    XATBDS
108100980327     C*
108200970725     C     XTBERR        IFEQ      *Off
108300980327     C                   movel     xtbuni        angg75ds
108400970729     C     'USRDTA'      Scan      OvrG75                                 21
108500970729     C                   if        *In21    =    *On
108600970729     C* Libro del registro da stampare
108700970729     C     'LLL'         Scan      OvrG75        $X                       21
108800970729     C                   if        *In21    =    *On
108900970729     C                   Eval      %SubSt(OvrG75:$X:3)  =  Lib163
109000021024D1220 *
109100021024  "   * se il nome del libro contiene dei blank, li sostituisco
109200021024  "   * con '_' per evitare che il comando dell OvrPrtF vada in errore
109300021024  "   *
109400021024  "   * es: LIBRO      |'L  '|'LL '|' L '|' LL'|'  L'|'L L'|
109500021024  "   *     ___________|_____|_____|_____|_____|_____|_____|
109600021024  "   *     nel comando|     |     |     |     |     |     |
109700021024  "   *     diventa    |'L  '|'LL '|'_L '|'_LL'|'__L'|'L_L'|
109800021024  "   *
109900021024  "  C                   if        %SubSt(OvrG75:$X:1) = *blank
110000021024  "  C                   eval      %SubSt(OvrG75:$X:1) = '_'
110100021024  "  C                   endif
110200021024  "  C                   if        %SubSt(OvrG75:$X+1:1) = *blank  and
110300021024  "  C                             %SubSt(OvrG75:$X+2:1) <> *blank
110400021024  "  C                   eval      %SubSt(OvrG75:$X+1:1) = '_'
110500021024  "  C                   endif
110600021024D1220 *
110700970725     C                   endif
110800970729     C                   endif
110900970725     C* Eseguo ovrprtf
111000980327     C                   exsr      CallX32
111100980327     C* msg avvertimento
111200970725     C                   else
111300970725     C                   ExSr      MsgPgm
111400980327     C                   endif
111500980327     C*
111600980327     C                   ENDSR
111700970409     C/EJECT
111800970725     C*----------------------------------------------------------
111900970725     C* Esegue ovrprtf
112000970725     C*----------------------------------------------------------
112100970725     C     CallX32       BEGSR
112200970725     C                   call      'X32OVRPRT'
112300970725     c                   parm                    OVRG75
112400970725     c*
112500970725     c                   ENDSR
112600970725     C/eject
112700970725     C******************************************************
112800970725     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
112900970725     C******************************************************
113000970725     C     MSGPGM        BEGSR
113100970725     C*
113200970725     C                   Z-ADD     7             MSGMSG
113300970725     C                   MOVEL     'N'           MSGRSP
113400970725     C                   MOVEL     'C'           MSGTPR
113500970725     C                   MOVEL     'S'           MSGEMV
113600970725     C                   Z-ADD     10            MSGRGP
113700970725     C                   MOVEL     'N'           MSGLCK
113800970725     C                   MOVEA     STATUS        STS
113900991108B0001c***  Soc163        cat       KeyGest:1     msgms1
114000991108B0001c     Soc163        cat       DSKEY:1       msgms1
114100970725     C                   MOVEL     'N'           MSGCNL
114200991104B0001C***                MOVEL     'N'           MSGVID
114300991104  "  C***                MOVEL     'N'           MSGOPE
114400991104  "   *
114500991104  "   * Si fa in modo di mandare i msg di errore all'operatore e
114600991104  "   * al terminale. Il msg viene reperito con XRTVM.
114700991104  "  C                   MOVEL     $MSGID        IDMSG
114800991104  "  C                   CALLB     'XRTVM'
114900991104  "  C                   PARM                    IDMSG            27
115000991104  "  C                   PARM                    TXTMSG          100
115100991104  "  C                   PARM                    ERRMSG            1
115200991104  "  C                   PARM                    *omit                          centratura
115300991104  "  C                   PARM                    *omit                          lun output.
115400991104  "  C     ERRMSG        IFNE      *ON
115500991104  "  C                   MOVEL     TXTMSG        MSGSND
115600991104  "  C                   ELSE
115700991104  "  C                   MOVEL     *ALL'?'       MSGSND
115800991104  "  C                   ENDIF
115900991104  "   *
116000991104  "  C                   MOVEL     'S'           MSGVID
116100991104  "  C                   MOVEL     'S'           MSGOPE
116200991104  "  C                   MOVEL     KPJBA         KPJBASav
116300991104B0001C                   MOVEA     KPJBA         JBA
116400970725     C                   MOVEL     'N'           MSGSTP
116500970725     C                   CALL      'XMSGR'
116600970725     C                   PARM                    XMSGDS
116700991104B0001 *
116800991104B0001C                   MOVEL     KPJBASav      KPJBA
116900970725     C*
117000970725     C                   ENDSR
117100990427     C/EJECT
117200980126     C************************************************************
117300980126     C* DECODIFICA CONTO
117400980126     C************************************************************
117500950710     C     CALLMVC002    BEGSR
117600950710     C*
117700950711     C                   clear                   ND002DS
117800950711     C                   eval      $LenOut = %Size(ND002DS)
117900950711     C*
118000950711     C                   CALLB     'NDMVC002'
118100950710     C                   PARM      ivasocieta    $soc
118200950710     C                   PARM      ivakcccp      $kcc
118300950710     C                   PARM      ivaksccp      $ksc
118400950710     C                   PARM                    $date
118500950710     C                   PARM      *off          $GesMsg
118600950710     C                   PARM      *off          $Esito
118700950710     C                   PARM      'ND002DS'     $Struttura
118800950710     C                   PARM                    ND002DS
118900950710     C                   PARM                    $LenOut
119000950710     C*
119100950710     C                   ENDSR
119200950710     C/EJECT
119300980126     C************************************************************
119400980126     C* DATI ANAGRAFICI SOGGETTO
119500980126     C************************************************************
119600950711     C     CALLDVASOG    BEGSR
119700950711     C*
119800950711     C                   move      '1'           DVARIC
119900950711     C                   move      ivasocieta    DVASOCIETA
120000950711     C                   move      *blank        DVAUNITA
120100950711     C                   movel     'DVASOG'      DVASTRUTT
120200950711     C                   eval      DVALENOUT = %Size(DVASOG)
120300950711     C                   move      sogg002       DVASOGG
120400950711     C*
120500950711     C                   CALLB     'NDDVASOG'    PDVASOG
120600950711     C*
120700950711     C                   ENDSR
120800950711     C/EJECT
120900980126     C************************************************************
121000980126     C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
121100980126     C************************************************************
121200950711     C     CALLDVACPI    BEGSR
121300950711     C*
121400951201     C                   move      '2'           DVCRIC
121500950711     C                   move      ivasocieta    DVCSOCIETA
121600950711     C                   move      *blank        DVCUNITA
121700960711     C                   movel     'ANCPI'       DVCSTRUTT
121800971021     C                   move      ivadtdoc      DVCDTRIF
121900950711     C                   eval      DVCLENOUT = %Size(ANCPI)
122000950711     C                   move      sogg002       DVCSOGG
122100950711     C*
122200950711     C                   CALLB     'NDDVACPI'    PDVACPI
122300950711     C*
122400950711     C                   ENDSR
122500950711     C/EJECT
122600950606     C************************************************************
122700950606     C* FINE PROGRAMMA
122800950606     C************************************************************
122900950606     C     ENDPGM        BEGSR
123000950606     C*
123100950703     C                   MOVEL     C163DS        KPJBU
123200950606     C                   SETON                                        LR
123300950606     C                   RETURN
123400950606     C*
123500950606     C                   ENDSR
123600950606     C/EJECT
123700950405     C************************************************************
123800900731     C* Definizioni e inizializzazioni
123900950405     C************************************************************
124000940831     C     *INZSR        BEGSR
124100000222     C*
124200000118C1309C     *DMY          Move      Udate         ZDateISO
124300000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
124400900731     C*
124500900731     C     *ENTRY        PLIST
124600900731     C                   PARM                    KPJBA
124700011214     c*
124800030307D1300c* chiamo programma generante tabella 008, il quale scrive in
124900011214  "  C* automatico i dati di tale tabella se vuota dal file ANDLI
125000030307  "  C* bisogna chiamarlo sempre, anche se non richiesta la stampa inte
125100030307  "  c* stazione, in quanto deve memorizzare nella tabella 008
125200030307  "  c* l'ultimo numero protocollo iva per libro
125300011214  "  C                   MOVEL     KPJBU         C163DS
125400030307  "  X***** EX C1529     if        int163 = *on
125500011214  "  c                   call      'ANG008R1'
125600011214  "  c                   parm                    soc163
125700170728D1300X***** EX C1529     endif
125701170728
125702170728       // Creo una table temporanea dove per ogni registro memorizzo l'ultimo
125703170728       // libro che stamperò.
125704170728
125705171117       EXEC SQL CREATE TABLE qtemp/ultimo_libro (registro CHAR(1)
125706171117       , libro CHAR(3));
125707170728
125708170728       IF sqlCode = -601;
125709170728         // Esiste già, no problem.
125710170728       ELSEIF sqlCode < *ZERO;
125711170728         DUMP(A);
125712170728         err163 = *ON;
125713170728         EXSR EndPgm;
125714170728       ELSE;
125715171120         EXEC SQL
125716171120         INSERT INTO qtemp/ultimo_libro
125717171120         SELECT ivaTpReg, MAX(ivaLibro) FROM ndivacgd
125718171120         WHERE ivaSocieta = :soc163 AND
125719171120         ( ivaDtAnnot BETWEEN :dti163 AND :dtf163 OR
125720171120             ivaDtRif BETWEEN :dti163 AND :dtf163 )
125725171120         GROUP BY ivaTpReg
125726171120         WITH NC;
125727170728         IF sqlCode < *ZERO;
125728170728           DUMP(A);
125729170728           err163 = *ON;
125730170728           EXSR EndPgm;
125731170728         ENDIF;
125732170728       ENDIF;
125733170728
125900940831     C                   ENDSR
126000980126     C/EJECT
126100950606     C************************************************************
126200950606     C* INIZIALIZZAZIONE VARIABILI
126300950606     C************************************************************
126400950606     C     INZVAR        BEGSR
126500950606     C*
126600950703     C                   MOVEL     KPJBU         C163DS
126700990427     C*
126800980803     C* klist per ANDLS
126900980821     C     K4DLS2        KLIST
127000980803     C                   KFLD                    DlsSocieta
127100980803     C                   KFLD                    DlsTpreg
127200980803     C                   KFLD                    DlsLibro
127300980821     C                   KFLD                    Dlsdtfival
127400990427     C*
127500980821     C* klist per ANDLS
127600980821     C     K3DLS2        KLIST
127700980821     C                   KFLD                    DlsSocieta
127800980821     C                   KFLD                    DlsTpreg
127900980821     C                   KFLD                    DlsLibro
128000990427     C*
128100980803     C* klist per NDIVA
128200990527     C     K4IVA1        KLIST
128300990527     C                   KFLD                    Iv1Sys
128400990527     C                   KFLD                    Iv1NrAsReg
128500990527     C                   KFLD                    Iv1NrRigaM
128600990527     C                   KFLD                    Iv1NrRiga
128700990427     C*
128800020326C1550X**klist per NDIVA08J
128900020326  "  X***  K4IVA8        KLIST
129000020326  "  X***                KFLD                    ivasocieta
129100020326  "  X***                KFLD                    ivatpreg
129200020326  "  X***                KFLD                    ivalibro
129300020326  "  X***                KFLD                    ivadtreg
129400020326  "  X***
129500020326  "  X***  K3IVA8        KLIST
129600020326  "  X***                KFLD                    ivasocieta
129700020326  "  X***                KFLD                    ivatpreg
129800020326C1550X***                KFLD                    ivalibro
129900020326C1550 *
130000020402  "   * klist per NDIVA11J
130100020402  "  C     K04IVA11      klist
130200020326  "  C                   kfld                    IVASocieta
130300020326  "  C                   kfld                    IVATpReg
130400020326  "  C                   kfld                    IVALibro
130500020326  "  C                   kfld                    IVADtAnnot
130600020402  "  C     K03IVA11      klist
130700020326  "  C                   kfld                    IVASocieta
130800020326  "  C                   kfld                    IVATpReg
130900020326C1550C                   kfld                    IVALibro
131000950710     C*
131100971211     C* klist per NDMOV01L
131200950710     C     K3MOV1        KLIST
131300950710     C                   KFLD                    movsys
131400950710     C                   KFLD                    movnrasreg
131500950710     C                   KFLD                    movnrrigam
131600950706     C*
131700971211     C* klist per ANALR01L
131800961202     C     k5alr1        KLIST
131900961202     C                   KFLD                    alrsocieta
132000961202     C                   KFLD                    alrtpreg
132100961202     C                   KFLD                    alrlibro
132200961202     C                   KFLD                    alraliq
132300961202     C                   KFLD                    alrrifiva
132400950710     C*
132500971211     C* klist per ANOPE01L
132600950706     C     K2OPE1        KLIST
132700950706     C                   KFLD                    opesocieta
132800950706     C                   KFLD                    opecausale
132900950711     C*
133000950711     C* PLIST per NDVASOG
133100950711     C     PDVASOG       PLIST
133200950711     C                   PARM                    DVARIC
133300950711     C                   PARM                    DVASOCIETA
133400950711     C                   PARM                    DVAUNITA
133500950711     C                   PARM                    DVASTRUTT
133600950711     C                   PARM                    DVADTRIF
133700950711     C                   PARM                    DVALENOUT
133800950711     C                   PARM                    *OMIT
133900950711     C                   PARM                    *OMIT
134000950711     C                   PARM                    *OMIT
134100950711     C                   PARM                    *OMIT
134200950711     C                   PARM                    DVASOGG
134300950711     C                   PARM                    DVATPIND
134400950711     C                   PARM                    DVACDIND
134500950711     C                   PARM                    DVAERRORE
134600950711     C                   PARM                    DVAOUTPUT
134700950711     C*
134800950711     C* PLIST per NDDVACPI
134900950711     C     PDVACPI       PLIST
135000950711     C                   PARM                    DVCRIC
135100950711     C                   PARM                    DVCSOCIETA
135200950711     C                   PARM                    DVCUNITA
135300950711     C                   PARM                    DVCSTRUTT
135400950711     C                   PARM                    DVCDTRIF
135500950711     C                   PARM                    DVCLENOUT
135600950711     C                   PARM                    DVCSOGG
135700950711     C                   PARM                    DVCERRORE
135800950711     C                   PARM                    DVCOUTPUT
135900950703     C*
136000950606     C* Reperimento azienda
136100950703     C                   MOVE      SOC163        SOCXSC
136200950606     C                   MOVEL     'SOC001'      TIPXSC
136300950606     C                   EXSR      REPSOC
136400950606     C     RTNXSC        IFNE      '1'
136500950606     C                   MOVEL     XSOCDS        SOC001
136600950606     C                   ELSE
136700950703     C                   MOVE      *ON           ERR163
136800950606     C                   EXSR      ENDPGM
136900950606     C                   ENDIF
137000030418C1651C*
137100030418  "  C* Reperimento parametro IVA (anagrafiche iva)
137200030418  "  C*
137300030418  "  C                   CALLB     'XPAR'
137400030418  "  C                   parm                    xscsoc
137500030418  "  C                   parm      'IVA     '    xpapar            8
137600030418  "  C                   parm                    xpaout
137700030418  "  C                   parm      *OFF          xpaerr            1
137800030418  "  C                   parm      ' '           xparic            1
137900030418  "  C     xpaerr        ifeq      *ON
138000030418  "  C                   MOVE      *ON           ERR163
138100030418  "  C                   EXSR      ENDPGM
138200030418  "  C                   else
138300030418  "  C                   movel     xpaout        ANP045DS
138400030418c1651C                   endif
138500950705     C*
138600950705     C* Valorizzazione campi univoci testate
138700950710     C                   MOVEL     KNSIF         NOMSIF
138800950705     C                   MOVEL     XSCDSI        NOMDIT
138900950705     C                   MOVEL     DSPGM         NOMPGM
139000950710     C* Reperisco nome sistema informatico
139100950710     C                   CALL      'XNETA'                              21
139200950710     C                   PARM                    NOMSYS
139300950707     C                   move      XSCCDF        codfis
139400950707     C                   move      lib163        t2libro
139500950707     C                   move      dbr163        t2desreg
139600000118C1309C******dmy          move      dti163        t2datai
139700000118C1309C     *JobRun       move      dti163        t2datai
139800000118C1309C******dmy          move      dtf163        t2dataf
139900000118C1309C     *JobRun       move      dtf163        t2dataf
140000990129     C                   move      xscdiv        t3mc1
140100990129     C                   move      xscdiv        t3mc2
140200950630     C*
140300950706     C     *like         define    ivanrreg      savnrreg
140400020402C1550C     *like         define    IVADtAnnot    SAVDtAnnot
140500990128     C     *like         define    ivanrdoc      savnrdoc
140600030218c1615C     *like         define    ivaSerDoc     savSerDoc
140700990128     C     *like         define    ivadtdoc      savdtdoc
140800980114     C     *like         define    ivadtrif      savdtrif
140900980114     C     *like         define    ivasosp       savsosp
141000950712     C*
141100950712     C* Spengo indicatori lavoro
141200951128     C                   setoff                                       607080
141300951218     C                   setoff                                       75
141400980609     C*
141500980609     C* Inizializzo flag di lavoro
141600980609     C                   move      *off          $primariga
141700030422     c*
141800030422     C* reperimento dati da tabella '008'
141900030422C1651C                   EXSR      RepTab008
142000011212     C*
142100950712     C*
142200990311     C* Se richiesto stampa solo totali e richiesto aggiornamento e
142300990311     C* il regime iva è trasportatori accendo un indicatore che impedirà
142400990311     C* la stampa del dettaglio dato che comunque si dovrà esegure la
142500990311     C* routine per poter eventualmente "slittare" la data liquidazione iva
142600030513C1651X****               if        tot163 = *on and agr163 = *on and
142700030513  "  X****                         XscIvaRgm = '1'
142800030513C1651C                   if        tot163 = *on
142900990311     C                   seton                                        85
143000990311     C                   else
143100990311     C                   setoff                                       85
143200990311     C                   endif
143300020327C1550 *
143400020327  "   * se regime iva trasp. mista l'intestazione delle righe
143500020327  "   * e la data è 'Annotazione'
143600020327  "  C                   if        XScIvaRgm = '2'
143700020327  "  C                   eval      *in81 = *on
143800020327  "  C                   endif
143900020327C1550 *
144000990311     C*
144100950606     C                   ENDSR
144200030422     c/EJECT
144300030422c1651C************************************************************
144400030422  "  C* Reperimento dati da tabella 008
144500030422c1651C************************************************************
144600030422     C     REPTab008     BEGSR
144700030422     c*
144800030422     C* Reperisco SEMPRE Ultimo Numero Protocollo del libro
144900030422     c                   select
145000030422     c                   when      tpr163='1'
145100030422     c                   eval      xpapar = 'LIBRIVAA'
145200030422     c                   when      tpr163='2'
145300030422     c                   eval      xpapar = 'LIBRIVAV'
145400030422     C                   other
145500030422     c                   eval      xpapar = 'LIBRIVAC'
145600030422     c                   endsl
145700030422     C                   CALLB     'XPAR'
145800030422     C                   parm                    xscsoc
145900030422     C                   parm                    xpapar            8
146000030422     C                   parm                    xpaout
146100030422     C                   parm      *OFF          xpaerr            1
146200030422     C                   parm                    xparic            1
146300030422     c                   movel     xpaout        ndc099ds
146400030422     C*
146500030422     c* aggancio tabella 008 PER LIBRO
146600030422     C                   if        xpaerr<>*on
146700030422     c                   reset                   xatbds
146800030422     C                   MOVE      soc163        XTBAZI
146900030422     C                   MOVE      '008'         XTBcod
147000030422     c                   eval      xtbkey = '00000000000' + tpr163 + lib163
147100030422     C                   MOVE      '1'           XTBRIC
147200030422     C                   MOVE      '0'           XTBALC
147300030422     C                   CALLB     'XATB'
147400030422     c                   parm                    xatbds
147500030422     c                   eval      ang008ds = xtbuni
147600030422     C*
147700030422     c                   if        %subst(ang008ds:27:9) = *blanks
147800030422     c                   eval      %subst(ang008ds:27:9) = *zeros
147900030422     c                   endif
148000030422     c                   if        %subst(ang008ds:21:6) = *blanks
148100030422     c                   eval      %subst(ang008ds:21:6) = *zeros
148200030422     c                   endif
148300030422     c* salvo numero /data registro
148400030422     c                   eval      SavNrReg8= NrReg008
148500030422     c                   eval      SavdtReg8= DtReg008
148600030422     c* se richiesta intestazione
148700030422     c                   if        int163=*on  AND
148800030422     C* se numerazione PER LIBRO
148900030422     c                             numeraz045 = *blanks
149000030422     c                   z-add     Pagina008     NumeroPag         6 0
149100030422     c                   endif
149200030422     c*
149300030422     c                   else
149400030422     c* arriva messaggio di errore da xpar all'operatore
149500030422     c                   eval      err163 = *on
149600030422     c                   exsr      endpgm
149700030422     c                   endif
149800030422     C*
149900030422     c* se richiesta intestazione
150000030422     c                   if        int163=*on  AND
150100030422     C* se numerazione PER REGISTRO
150200030422     c                             numeraz045 = '1'
150300030508     c* se bollato
150400030508     c                   if        bol163=*on
150500030508     c* o se non bollato ma numero 0
150600030508     c                             or bol163 <> *on and pag163 = 0
150700030508     c* prendo numero da tabella
150800030422     c                   reset                   xatbds
150900030422     C                   MOVE      soc163        XTBAZI
151000030422     C                   MOVE      '008'         XTBcod
151100030422     c                   eval      xtbkey = '00000000000' + tpr163 + '   '
151200030422     C                   MOVE      '1'           XTBRIC
151300030422     C                   MOVE      '0'           XTBALC
151400030422     C                   CALLB     'XATB'
151500030422     c                   parm                    xatbds
151600030422     c                   eval      ang008ds = xtbuni
151700030422     C*
151800030422     c                   if        %subst(ang008ds:27:9) = *blanks
151900030422     c                   eval      %subst(ang008ds:27:9) = *zeros
152000030422     c                   endif
152100030422     c                   if        %subst(ang008ds:21:6) = *blanks
152200030422     c                   eval      %subst(ang008ds:21:6) = *zeros
152300030422     c                   endif
152400030422     C*
152500030422     c                   z-add     Pagina008     NumeroPag
152600030508     c* se non bollato
152700030508     c                   else
152800030508     c* uso numero che arriva
152900030508     c                   z-add     Pag163        NumeroPag
153000030508     c                   endif
153100030508     c*
153200030422     c                   endif
153300030422     C*
153400030422c1651C                   ENDSR
153500950606     C/EJECT
153600980126     C************************************************************
153700980126     C* Reperimento dati società
153800980126     C************************************************************
153900941103     C     REPSOC        BEGSR
154000980126     C*
154100950630     C                   CALLB     'XSOC'
154200941103     C                   PARM                    TIPXSC            6
154300941103     C                   PARM                    SOCXSC            3
154400941103     C                   PARM                    CDSXSC            9 0
154500941103     C                   PARM                    MODXSC            3
154600941103     C                   PARM      *BLANKS       RTNXSC            1
154700941103     C                   PARM                    XSOCDS
154800941103     C                   PARM                    KPJBA
154900980126     C*
155000941103     C                   ENDSR
155100981218     C/EJECT
155200981218     C************************************************************
155300981218     C* Reperimento dati estensione nominativi partite
155400981218     C************************************************************
155500981218     C     RepEstNomP    BEGSR
155600981218     C*
155700981218     C                   Clear                   NDCPADS
155800981218     C                   Eval      LenOut = %Size(NDCPADS)
155900981218     C*
156000981218     C                   CALL      'NDC066R2'
156100981218     C                   PARM      IVASys        Sys
156200981218     C                   PARM      0             NrAsInt
156300981218     C                   PARM      IVANrAsReg    NrAsGen
156400981218     C                   PARM                    LenOut
156500981218     C                   PARM                    NDCPADS
156600981218     C                   PARM      '0'           Esito
156700981218     C*
156800981218     C                   If        Esito = '0'
156900981218     C                   MoveL     CPADescr      P1Descr
157000981218     C                   If        CPAPartIva = *Blank
157100981218     C                   MoveL     CPACdFisc     P1PartIva
157200981218     C                   Else
157300981218     C                   MoveL     CPAPartIva    P1PartIva
157400981218     C                   EndIf
157500981218     C                   MoveL     CPAIndiriz    P1Indiriz
157600981218     C                   MoveL     CPACap        P1Cap
157700981218     C                   MoveL     CPALocalit    P1Localit
157800981218     C                   MoveL     CPAProv       P1Prov
157900981218     C                   MoveL     CPAStato      P1Stat
158000981218     C* decodifica stato
158100981218     C                   Clear                   ANGG12DS
158200981218     C                   MOVE      '1'           XTBRIC
158300981218     C                   MOVE      Soc163        XTBAZI
158400981218     C                   MOVE      *ZERO         XTBKEY
158500981218     C                   MOVE      'G12'         XTBCOD
158600981218     C                   MOVE      CPAStato      XTBKEY
158700011214     C                   CALLB     'XATB'
158800981218     C                   PARM                    XATBDS
158900981218   B3C     XTBERR        IFEQ      '0'
159000981218     C                   MOVEL     XTBUNI        ANGG12DS
159100981218     C                   MoveL     DesG12        P1StaD
159200981218     C                   If        NazG12 = *Blank
159300981218     C                   Move      *Blank        P1PartIva
159400981218     C                   EndIf
159500981218   E2C                   ELSE
159600981218   E2C                   Move      *Blank        P1stat
159700981218   E2C                   Move      *Blank        P1staD
159800981218   E2C                   ENDIF
159900981218     C                   EndIf
160000981218     C*
160100981218     C                   ENDSR
160200130620
160300130620      * Controllo importo minimo fattura
160400130620      * DPR n. 695 del 1996 Art. 6 Comma 1
160500130620     C     yChkImpMin    BEGSR
160600130620      *
160700130620     C                   EVAL      yFatImpMin = '0'
160800130620      * Solo vendite.
160900130620     C                   IF        ivatpreg = '2'
161000130620      * Reperisco l'importo minimo.
161100130620     C                   CLEAR                   ynimf00f
161200130620     C                   CALLB     'YCO1632R'
161300130620     C                   PARM      ivatpreg      ytpreg
161400130620     C                   PARM      xscdiv        ydivisa
161500130620     C                   PARM      ivadtreg      ydtval
161600130620     C                   PARM                    ynimf00f
161700130620     C                   PARM      *BLANKS       yesito
161800130620      *
161900130620     C                   IF        yesito = '0'
162000130620>>>>>C                             AND
162100130620>>>>>C                             %ABS(YCO1636R(IvaSys:IvaNrAsReg:0:0))
162200130620>>>>>C                             < ImfImpMin
162300130620     C                   EVAL      yFatImpMin = '1'
162400130620     C                   ENDIF
162500130620      * Se la fattura è sotto l'importo minimo, scrivo YNFIM00F;
162600130620      * altrimenti cancello.
162700130620     C                   IF        yFatImpMin = '1'
162800130620     C                   EVAL      yopzione = '1'
162900130620     C                   ELSE
163000130620     C                   EVAL      yopzione = '4'
163100130620     C                   ENDIF
163200130620      *
163300130620     C                   CALLB     'YCO1633R'
163400130620     C                   PARM                    yopzione
163500130620     C                   PARM      ivasys        ysys
163600130620     C                   PARM      ivanrasreg    ynrasreg
163700130620     C                   PARM      *BLANKS       yesito
163800130620      *
163900130620     C                   ENDIF
164000130620      *
164100130620     C                   ENDSR
164200130620
164300130620      * DPR n. 695 del 1996 Art. 6 Comma 1
164400130620     C     yStpRie695    BEGSR
164500130620      * Stampa riepilogo in registro IVA.
164600130620     C                   IF        Bol163 = *ON
164700130620     C                   EVAL      kpjbu = c163ds
164800130620     C                   EVAL      yOpCode = 'INIZIO'
164900130620     C                   DOU       yRtnCode = 'FINE'
165000130620     C                   CALLB     'YCO1634R'
165100130620     C                   PARM                    kpjba
165200130620     C                   PARM                    yOpCode
165300130620     C                   PARM      *BLANK        yRtnCode
165400130620     C     tpImponib     PARM      tpImponib     yImponibile
165500130620     C     tpImporto     PARM      tpImporto     yImposta
165600130620     C     *IN40         PARM      *IN40         yIndOvrFlw
165700130620     C                   SELECT
165800130620     C                   WHEN      yRtnCode = 'FINE'
165900130620     C                   LEAVE
166000130620     C                   WHEN      yRtnCode = 'OVRFLW'
166100130620     C                   EXSR      testa
166200130620     C                   OTHER
166300130620     C                   LEAVE
166400130620     C                   ENDSL
166500130620     C                   EVAL      yOpCode = 'CONTINUA'
166600130620     C                   ENDDO
166700130620     C                   ENDIF
166800130620      * Stampa documento riepilogativo.
166900130620     C                   EVAL      kpjbu = c163ds
167000130620     C                   CALLB     'YCO1635R'
167100130620     C                   PARM                    kpjba
167200130620      *
167300130620     C                   ENDSR
167400130620     C/EJECT
167500130620     C************************************************************
167600130620     C* Annullamento riga
167700130620     C************************************************************
167800130620     C     YAnnRiga      BEGSR
167900130620      *-----------------------
168000130620     C                   EVAL      NRigAc = *ALL'*'
168100130620     C                   EVAL      %SUBST(NRIGAC
168200130620     C                             :89:19)
168300130620     C                             = Annullato
168400130620
168500130620     C                   EVAL      NRigVe = *ALL'*'
168600130620     C                   EVAL      %SUBST(NRIGVE
168700130620     C                             :89:19)
168800130620     C                             = Annullato
168900130620      *
169000130620     C                   IF        Tpr163 = '1'
169100130620     C                   DOW       CurLineA < 66
169200130620     C                   WRITE     YAnnAcq
169300130620     C                   ENDDO
169400130620     C                   ENDIF
169500130620      *
169600130620     C                   IF        Tpr163 = '2'
169700130620     C                   DOW       CurLineV < 66
169800130620     C                   WRITE     YAnnVen
169900130620     C                   ENDDO
170000130620     C                   ENDIF
170100130620      *
170200130620     C                   ENDSR
170201161129>>>>>
170202161129>>>>>  // --------------------------------------------------
170203161129>>>>>  // Procedure name: SendToDocFlow
170204161129>>>>>  // Purpose:        Invia lo spool a DocFlow
170205161129>>>>>  // Returns:        Esito.
170206161129>>>>>  // --------------------------------------------------
170207161129>>>>>
170208161129>>>>>  DCL-PROC SendToDocFlow ;
170209161129>>>>>
170210161129>>>>>    DCL-PI *N INT(10);
170211161129>>>>>    END-PI ;
170212161129>>>>>
170213161129>>>>>    DCL-DS local QUALIFIED;
170214161129>>>>>      aplKey CHAR(15);
170215161129>>>>>      filNam CHAR(10);
170216161129>>>>>      numFil CHAR(6);
170217161129>>>>>      fileOut CHAR(255);
170218161129>>>>>      subFldr CHAR(50);
170219161129>>>>>      esito CHAR(1);
170221161129>>>>>    END-DS;
170222161129>>>>>
170223171117>>>>>    // Non creo il file per DocFlow quando non è una stampa definitiva.
170226161129>>>>>
170227171117>>>>>    IF bol163 = *OFF;
170228161129>>>>>      RETURN *ZERO;
170229161129>>>>>    ENDIF;
170243170727>>>>>
170244161129>>>>>    SELECT;
170245161129>>>>>      WHEN tpr163 = '1';
170246161129>>>>>        local.aplKey = 'PJIVAACQUISTI';
170247161129>>>>>        local.subFldr = 'IVAA';
170248170721>>>>>        local.fileOut = 'IVAA';
170249161129>>>>>        local.filNam = fileNameA;
170250161129>>>>>        local.numFil = %EDITC(%DEC(spoolNumA:6:0):'X');
170251161129>>>>>        CLOSE ndIvaA198;
170252161129>>>>>      WHEN tpr163 = '2';
170253161129>>>>>        local.aplKey = 'PJIVAVENDITE';
170254161129>>>>>        local.subFldr = 'IVAV';
170255170721>>>>>        local.fileOut = 'IVAV';
170256161129>>>>>        local.filNam = fileNameV;
170257161129>>>>>        local.numFil = %EDITC(%DEC(spoolNumV:6:0):'X');
170258161129>>>>>        CLOSE ndIvaV198;
170259170727>>>>>      WHEN tpr163 = '3';
170260170727>>>>>        local.aplKey = 'PJIVACORRISPETT';
170261170727>>>>>        local.subFldr = 'IVAC';
170262170727>>>>>        local.fileOut = 'IVAC';
170263170727>>>>>        //local.filNam = fileNameC;
170264170727>>>>>        //local.numFil = %EDITC(%DEC(spoolNumC:6:0):'X');
170265170727>>>>>        CLOSE ndIvaC198;
170266161129>>>>>      OTHER;
170267161129>>>>>        RETURN *ZERO;
170268161129>>>>>    ENDSL;
170269171120>>>>>
170270171120>>>>>    // Non creo il file per DocFlow quando ho stampato niente.
170271171120
170272171120         CLEAR qusec;
170273171120         qusbprv = %SIZE(qusec);
170274171120
170275171120         RetrieveSpooledFileAttributes( qusa010001 : %SIZE(qusa010001)
170276171120         : 'SPLA0100' : '*' : *BLANK : *BLANK : local.filNam
170277171120         : %INT(local.numFil) : qusec );
170278171120
170279171120         SELECT;
170280171120           WHEN qusei = *BLANK;
170281171120           WHEN qusei = 'CPF3344'; // Il file non è più nel sistema.
170282171120>>>>>        RETURN *ZERO;
170283171120           OTHER;
170284171120             DUMP(A);
170285171120         ENDSL;
170286161129>>>>>
170287170727>>>>>    // Copio i dati di ogni libro IVA in aggiunta ad un file temporaneo;
170288170727>>>>>
170289170727>>>>>    local.fileOut = %TRIMR(local.fileOut) + soc163 + %CHAR(dtf163);
170290170727>>>>>
170291170727>>>>>    PrintFormattedJobLogData( 'Esportazione in DocFlow: applicazione %s +
170292170728>>>>>    file spool %s libro %s in file %s sottocartella %s ...' + LINEFEED
170293170728>>>>>    : %TRIMR(local.aplKey) : %TRIMR(local.filNam) : lib163
170294170728         : %TRIMR(local.fileOut) : %TRIMR(local.subFldr) );
170295170727>>>>>
170296170727>>>>>    TRBM_SendSpoolFileToIfs( local.aplKey : *BLANK : local.filNam
170297170727>>>>>    : psJobName : psJobUser : psJobNbr : local.numFil : *BLANK
170298170727>>>>>    : local.fileOut : local.subFldr : TRBM_CREATE_SUBFOLDER_YES
170299170727>>>>>    : TRBM_ADD_RECORDS : TRBM_LOG_YES : local.esito );
170300170727>>>>>
170301170727>>>>>    IF local.esito = TRBM_RETURN_ERROR;
170302170727>>>>>      DUMP(A);
170303170727>>>>>      RETURN -1;
170304170727>>>>>    ENDIF;
170305170727>>>>>
170306170727>>>>>    // Dopo aver copiato l'ultimo libro assegno al file il nome definitivo.
170307170727>>>>>
170308170727>>>>>    IF IsUltimoLibro(soc163 : tpr163 : lib163 : dti163 : dtf163);
170309170727>>>>>      PrintFormattedJobLogData( 'Esportazione in DocFlow: applicazione %s +
170310170728>>>>>      file spool %s in file %s sottocartella %s conferma finale ...'
170311170728           + LINEFEED : %TRIMR(local.aplKey) : %TRIMR(local.filNam)
170312170728           : %TRIMR(local.fileOut) : %TRIMR(local.subFldr) );
170313170727>>>>>      TRBM_SendSpoolFileToIfs( local.aplKey : *BLANK : local.filNam
170314170727>>>>>      : psJobName : psJobUser : psJobNbr : local.numFil : *BLANK
170315170727>>>>>      : local.fileOut : local.subFldr : TRBM_CREATE_SUBFOLDER_YES
170316170727>>>>>      : TRBM_FINE_ADD : TRBM_LOG_YES : local.esito );
170317170727>>>>>      IF local.esito = TRBM_RETURN_ERROR;
170318170727>>>>>        DUMP(A);
170319170727>>>>>        RETURN -1;
170320170727>>>>>      ENDIF;
170321170727>>>>>    ENDIF;
170322161129>>>>>
170323161129>>>>>    RETURN *ZERO;
170324161129>>>>>
170325161129>>>>>  END-PROC ;
170326161129>>>>>
170327170727
170328170727       // --------------------------------------------------
170329170727       // Procedure name: IsUltimoLibro
170330170727>>>>>  // Purpose:        Restituisce *ON quando è l'ultimo libro stampato.
170331170727>>>>>  // Returns:        *ON = Ultimo libro.
170332170727       // Parameter:      prmSocieta => ID società.
170333170727       // Parameter:      prmRegistro => Registro IVA.
170334170727>>>>>  // Parameter:      prmLibro => Libro IVA.
170335170727       // Parameter:      prmDataInizio => Data inizio.
170336170727       // Parameter:      prmDataFine => Data fine.
170337170727       // --------------------------------------------------
170338170727
170339170727       DCL-PROC IsUltimoLibro ;
170340170727
170341170727>>>>>    DCL-PI *N IND;
170342170727           prmSocieta CHAR(3) CONST;
170343170727           prmRegistro CHAR(1) CONST;
170344170727>>>>>      prmLibro CHAR(3) CONST;
170345170727           prmDataInizio DATE(*ISO) CONST;
170346170727           prmDataFine DATE(*ISO) CONST;
170347170727         END-PI ;
170348170727
170349170727>>>>>    DCL-S ultimoLibro CHAR(3) STATIC;
170350170727
170351171117>>>>>    EXEC SQL SELECT libro INTO :ultimoLibro FROM qtemp/ultimo_libro
170352171117         WHERE registro = :prmRegistro;
170353170727
170354170727         SELECT;
170355170727           WHEN sqlCode < *ZERO;
170356170727             DUMP(A);
170357170727>>>>>        RETURN *OFF;
170358170727         ENDSL;
170359170727
170360170727>>>>>    RETURN (ultimoLibro = prmLibro);
170361170727
170362170727       END-PROC ;
170363170727
