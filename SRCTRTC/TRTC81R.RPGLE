000100131014     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200930128     H DECEDIT('0,') DATEDIT(*DMY.)
000300131014      ***********************************************************************
000400131014      *   ESPORTA STATO DELLE CONSEGNE VERSO E.D.I.   ESTERO                *
000500131014      ***********************************************************************
000600960528     F*---------------------------------------------------------------------*
000700960528     F*  DATA BASE                                                          *
000800960528     F*---------------------------------------------------------------------*
000900050120     Fazorg01l  IF   E           K DISK
001000050120     F*---------
001100080418     F*****FNVAC00T  IF   E           K DISK
001200080418     FTivgd01L  uF   E           K DISK
001300100304     F                                     INFDS(vgdDS)
001400100304     FTivgd00f  uF   E             DISK    RENAME(TIVGD000:TIVGDfis)
001500960528     F*---------
001600050309     Ftigcp01L  IF   E           K DISK
001700961204     F*---------
001800050309     Ftignp01L  IF   E           K DISK
001900960528     F*---------
002000970319     FFNLBL01L  IF   E           K DISK
002100970319     F*---------
002200970319     FFNLBL02L  IF   E           K DISK
002300970319     F                                     RENAME(FNLBL000:FNLBL002)
002400970319     F*---------
002500060213     Ffiar401L  IF   E           K DISK
002600960528     F*---------
002700960528     FFNBLP01L  IF   E           K DISK
002800960528     F*---------
002900970304     FFNBLT01L  UF   E           K DISK
003000970319     F*---------
003100051017     FFIARS01L  IF   E           K DISK
003200980219      *---------
003300990810     FFNEVB04L  IF   E           K DISK
003400980204     F*---------
003500990810     FFNEVB05L  IF   E           K DISK
003600990810     F                                     RENAME(FNEVB000:FNEVB005)
003700960528     F*---------
003800080418     FEDIFTSTA  iF A E             DISK
003900960628     F*---------
004000980204     FEDRDE01L  UF   E           K DISK    USROPN
004100960528     F*---------
004200960726     FEDTAB01L  UF   E           K DISK
004300961029     F*---------
004400961029     FTABEL00F  IF   E           K DISK
004500990604     F*---------
004600040715     FFNDCT01L  UF   E           K DISK
004700040715     F****FNDCT02L  UF   E           K DISK
004800990604     FFNDCD01L  IF   E           K DISK
004900040607      *--
005000040607      *   x status 20/210 -> Supermercato
005100131014     Ffiarbf2c  IF   E           K DISK
005200040607      *--
005300100304      *
005400100304      * numero relativo di record
005500100304     D  vgdDS          DS
005600100304     D  NrRECvgd             397    400B 0
005700100304      *
005800960528     D*---------------------------------------------------------------------*
005900960528     D* Schiere
006000960528     D*---------------------------------------------------------------------*
006100960528     D* Schiera per scrittura dati località record ST10
006200960528     D* Schiera per scrittura testi liberi record ST15
006300960528     D* Schiera per scrittura testi liberi record SD15
006400960528     D* Schiera per reperimento nr.seganacollo
006500970625     D SG1             S             35    DIM(5000)                            Sgn.conse.
006600970625     D SG2             S             35    DIM(5000)                            Sgn.no con.
006700970625     D SG3             S             35    DIM(5000)                            Sgn.eve.pid
006800970625     D SG4             S             35    DIM(5000)                            IDD NO FL1
006900960528     D* Tabella codici eventi
007000961022     D CEV             S              3    DIM(200)
007100961022     D STS             S              3    DIM(200)
007200961022     D CDV             S              3    DIM(200)
007300961022     D STV             S              3    DIM(200)
007400960528     D C2A             S              3    DIM(200)
007500970319     D* Codici eventi IDD
007600970319     D IDD             S              3    DIM(200)
007700050909     D* Codici Giacenze/Lasciati Avvisi
007800050922     D GiaC            S              3    DIM(200)
007900050926     D LAvv            S              3    DIM(200)
008000960528     D* Tabella partner
008100070503     D CPT             S             35    DIM(300)
008200070503     D CPU             S             35    DIM(300)
008300070503     D DPT             S             90    DIM(300)
008400070503     D KSC             S              7  0 DIM(300)
008500960821     D* TESTO LIBERO
008600970318     D***                 FTX     1   1 70
008700960912     D* composizione schiera 35 chr x segnacollo
008800960912     D C35             S              1    DIM(35)
008900020418     D* Schiera x cariamento filiali gestite su AS
009000030926     D****** £1              S              3  0 DIM(30)                              Fil. gestite
009100030926     D  Pou            s              3  0 INZ
009200030926     D                                     DIM(250)
009300030926
009400980126     D* Esecuzione QCMD x controllare se esiste un CHKOBJ
009500990607     D CMD             S             45    DIM(1) CTDATA PERRCD(1)              QCAEXEC
009600960528     D*---------------------------------------------------------------------*
009700960528     D* DS
009800960528     D*---------------------------------------------------------------------*
009900960528     D* .. per scompozione dati da spedire a seconda del tipo record
010000960528     D EDST00        E DS                  EXTNAME(EDST00DS)
010100960708     D EDST05        E DS                  EXTNAME(EDST05DS)
010200960528     D EDST10        E DS                  EXTNAME(EDST10DS)
010300960528     D  T10                   48    103
010400960528     D                                     DIM(2)
010500960528     D EDST15        E DS                  EXTNAME(EDST15DS)
010600960528     D  T15                   13    362
010700960528     D                                     DIM(5)
010800960528     D EDSD00        E DS                  EXTNAME(EDSD00DS)
010900960528     D EDSD05        E DS                  EXTNAME(EDSD05DS)
011000960528     D EDSD10        E DS                  EXTNAME(EDSD10DS)
011100960528     D EDSD15        E DS                  EXTNAME(EDSD15DS)
011200960528     D  D15                   13    362
011300960528     D                                     DIM(5)
011400960528     D EDSD20        E DS                  EXTNAME(EDSD20DS)
011500960730     D  D20                   33    732
011600960730     D                                     DIM(20)
011700960528     D*  Ds per scrittura dati località
011800960528     D WST10           DS
011900960528     D  WTPL                   1      3
012000960528     D  WLOC                   4     28
012100960528     D*---------------------------------------------------------------------*
012200960528     D* .. per reperimento tipo record di EDIFTSTA da scrivere
012300960528     D*                    .. 5 - 8  : tipo record
012400040218     D WDAT            DS          1950
012500960729     D  STAPRG                 1      4  0
012600960726     D  STATPR                 5      8
012700960528     D*---------------------------------------------------------------------*
012800080418     D fnVACds       E DS
012900960528     D KPJBA         E DS
013000020412     D UT§DSE0F      E DS
013100960528     D CNCR80        E DS
013200050120     D OG143         E DS
013300050120     D ds1B          E DS
013400091117     D ds3K          E DS
013500020412     D EDIDS2A       E DS
013600970319     D DS2A          E DS
013700020412     D EDIDSPT       E DS
013800020412     D EDIDSPU       E DS
013900020412     D EDIDSMS       E DS
014000020412     D DDCT01        E DS
014100960528     D*  DS x scomposizione segnacollo
014200960528     D DSSGN           DS
014300961029     D  SGNFLS                 1      3  0
014400960528     D  SGNLNA                 4      6  0
014500960528     D  SGNNRS                 7      8  0
014600960528     D  SGNNSC                 9     15  0
014700960829     D  SGNZNC                16     17  0
014800960528     D WLBDA8          DS
014900960528     D  G02DAT                 1      8  0
015000960528     D  G02INV                 9     16  0
015100960528     D  G02ERR                17     17
015200960528     D  G02TGI                18     22  0
015300990604      *
015400991019      * DS numero spedizione (da impostare x Europolitan)
015500980122     D DATSPE          DS
015600040305     D  CNI_SPE                3     16  0
015700080418     D   cni_VACAAS            1      4  0
015800080418     D   cni_VACLNP            5      7  0
015900080418     D   cni_VACNRS            8      9  0
016000080418     D   cni_VACNSP           10     16  0
016100080418     D   cni_VACLNA           17     19  0
016200980122     D  FILLER                20     20
016300040715
016400040715     d Fidn12ds      e ds
016500040715     d  skKey                 26    305    dim(20)
016600040715     d  skAnn                306    325    dim(20)
016700040715     d  skDca                326    485    dim(20)
016800040715     d  skFca                486    545  0 dim(20)
016900040715     d  skDch                546    705  0 dim(20)
017000040715     d  skCch                706    745    dim(20)
017100040715
017200040715     d dsKey           ds
017300040715     d  dsaac                         4  0
017400040715     d  dsfil                         3  0
017500040715     d  dsnca                         7  0
017600040715
017700040715     D Kaac            S                   LIKE(dctaac)
017800040715     D Kfil            S                   LIKE(dctfil)
017900040715     D Knca            S                   LIKE(dctnca)
018000020418
018100030926      ******** ds per trul06r
018200030926     d****** trul06ds      e ds
018300030926     d******  lin                    1     90  0 dim(30)
018400030926     D Tibs56ds      E DS
018500030926     D                                     INZ
018600030926     D  Ski                           3  0
018700030926     D                                     DIM(250)
018800030926     D                                     OVERLAY(O56Ski)
018900030926
019000040607     d variata         s              1    INZ('N')
019100081112     d Bolla_variata   s              1    INZ('N')
019200020418
019300960708     D*
019400961122     D COST1           C                   CONST('IT/BAR/IT1   ')
019500060503     D CALB_Plat       C                   CONST('039IT04507990150')
019600960528     I*---------------------------------------------------------------------*
019700960528     I* FILE
019800960528     I*---------------------------------------------------------------------*
019900960528     IIFTSTA00
020000960528     I              SSIFTSTA                    STADAT
020100040607
020200040607     I* GLI INDICATORI DI TIPO RECORD x file Combinato
020300040607     IFNARBD00      11
020400040607     IFNARBK00      12
020500040607     IFIARBT00      13
020600040607     IFNARBG00      14
020700040607     IFNARBM00      15
020800040607     IFNARBV00      16
020900040607     IFNARBP00      17
021000131014     IFNARBN00      18
021100040607      *  INDICI PER:
021200040607      *  - ANNO
021300040607      *  - LINEA PARTENZA
021400040607      *  - SERIE
021500040607      *  - NUMERO SPEDIZIONE
021600040607      *  - DATA VARIAZIONE
021700040607      *  - ORA VARIAZIONE
021800000201      *---------------------------------------------------------------------*
021900000201      * Ciclo principale
022000000201      *---------------------------------------------------------------------*
022100070503      *   Se non è stata trovata la PT nella INZSR deve chiudere il PGM  ?
022200070503     C                   if        WFINE = 'S'
022300070503     c                   goto      fine
022400070503     c                   end
022500080418      *
022600080418      * Posizionamento sul TIVGD x il nuovo ciclo di lettura
022700080418     C                   EXSR      POSFIL
022800000201      *
022900960528      * Posizionamento iniziale sul file  (prima lettura)
023000960528     C                   EXSR      REDFIL
023100000201      *
023200000201      * TESTATA Messaggio
023300000201     C                   if        WFINE <> 'S'
023400000201     C                   EXSR      TESTAMSG
023500000201     C                   endif
023600980220      *
023700980220      * Loop letture FNVAC e scrittura record con dati singola bolla
023800000201     C     WFINE         DOWNE     'S'
023900000201      *
024000000201      * Se ho scritto 999 spedizioni devo scrivere un nuovo msg
024100000201     C                   if        WPROG = 999
024200980223      * Caricamento NUMERATORE
024300960927     C                   EXSR      CARNUM
024400000201      * TESTATA Messaggio
024500000201     C                   EXSR      TESTAMSG
024600000201     C                   endif
024700000201      *
024800000201      *  DETTAGLIO Messaggio
024900000201      *
025000980220      * Eseguo preparazione dati per scrittura record SD00
025100960528     C                   EXSR      WRSD00
025200980220      * Eseguo scrittura dati record SD05 e SD00
025300980220     C                   MOVEL     'N'           WRTEVE            1
025400960528     C                   EXSR      WRSD05
025500980220      *
025600980220      * Scrivo tipi record successivi solo se ho scritto SD00 e SD05
025700980220     C     WRTEVE        IFEQ      'S'
025800980220      * Eseguo scrittura dati record SD10
025900960528     C                   EXSR      WRSD10
026000960528      * Eseguo scrittura dati record SD15
026100960528     C                   EXSR      WRSD15
026200960528      * Eseguo scrittura dati record SD20
026300960528     C                   EXSR      WRSD20
026400980220     C                   END
026500100304      *
026600100304      * Se arrivato qui tutto OK
026700100304      * Aggancia il record fisico per certificare che è andato tutto bene
026800100304      *  e che ha elaborato correttamente il record trasformandolo in EDI.
026900100304     c                   clear                   elab10           10 0
027000100304     c     NrRECvgd      chain     tiVGD00F
027100100304     c                   if        %Found(tiVGD00F)
027200100304     c                   eval      Elab10 = WoggiYMD * 10000 + wHHMM
027300100304     c                   movel     Elab10        vgdPRG
027400100304     c                   update    tiVGDfis
027500100304     c                   end
027600100304      *
027700980220      * Lettura successiva
027800961007     C     NEWREC        TAG
027900960528     C                   EXSR      REDFIL
028000000201     C                   ENDDO
028100980220      * Fine pgm.
028200070503     c     fine          tag
028300000201     C                   eval      *inlr = *on
028400080418      *----------------------------------------------------------------
028500080418      *? POSFIL - Si Posiziona x la LETTURA PRIMO RECORD
028600080418      *----------------------------------------------------------------
028700080418     C     POSFIL        BEGSR
028800080418      *
028900080418      *? Si posiziona con il tipo VC e cliente + l'invio EDI...  ?
029000080418     C     KEY_VGD01     setll     tiVGD01L
029100080418      *
029200080418     C                   ENDSR
029300980220      *----------------------------------------------------------------
029400980220      *? REDFIL - LETTURA PROSSIMO RECORD
029500980220      *----------------------------------------------------------------
029600960528     C     REDFIL        BEGSR
029700980220      *
029800990326     C     LEGGI         TAG
029900990326      *
030000080418      * Lettura file VAC
030100080418     C***********        READ      FNVAC00T                               30
030200080418     C     KEY_VGD01     reade     tiVGD01L                               30
030300990326      *
030400000201     C                   If        *IN30 = *on
030500000201     C                   MOVEL     'S'           WFINE             1
030600000201     C                   Else
030700090219      *
030800090219      *? DEVE LEGGERE solo i record con TIPO invio "EW" da mandare via EDI
030900090219     c                   if        vgdTSC <> 'EW'
031000090219     c                   goto      LEGGI
031100090219     c                   end
031200990326      *
031300080418      *? Una volta letto il TIVGD imposta la relativa DS del VAC...  ?
031400080418     c                   eval      fnVACds = vgdDTA
031500080418      *
031600080418      *? poi aggiorna come già letto il record ...  ?
031700080418     c                   eval      VGDSTO ='S'
031800100304     c                   eval      VGDCNT = VGDCNT + 1
031900080418      *
032000080418      *?                ====================  ?
032100080418     c                   update    tivgd000
032200080418      *?                ====================  ?
032300080418      *
032400051116      *  Pulizia campi riferimento di giacenze della precedente lettura
032500051116     c                   clear                   GCPAGC
032600051116     c                   clear                   GCPFGC
032700051116     c                   clear                   GCPNGC
032800051116      *
032900990326      * Non elaboro codice anomalia IDD3 trasmesso dall'arrivo
033000000201     C                   if        VACCCA = '5'
033100990326     C                   GOTO      LEGGI
033200000201     C                   endif
033300990922      *
033400990922      * Non elaboro SE NON TROVO RCD DI FNBLP
033500990922     C                   Z-ADD     VACAAS        KAAS
033600990922     C                   Z-ADD     VACLNP        KLNP
033700990922     C                   Z-ADD     VACNRS        KNRS
033800990922     C                   Z-ADD     VACNSP        KNSP
033900990922     C     KBLP          CHAIN     FNBLP01L                           31
034000000201     C   31              GOTO      LEGGI
034100000201      *
034200000201     C                   Endif
034300980220      *
034400960528     C                   ENDSR
034500000201      *----------------------------------------------------------------
034600000201      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO
034700000201      *----------------------------------------------------------------
034800000201     C     TESTAMSG      BEGSR
034900000201      *
035000000201      * Eseguo scrittura dati record ST00
035100000201     C                   EXSR      WRST00
035200000201      * Eseguo scrittura dati record ST05
035300000201     C                   EXSR      WRST05
035400000201      * Eseguo scrittura dati record ST10
035500000201     C***                EXSR      WRST10
035600000201      * Eseguo scrittura dati record ST15
035700000201     C                   EXSR      WRST15
035800000201      *
035900000201     C                   ENDSR
036000980629      *----------------------------------------------------------------
036100980629      *? WRST00 - Scrittura record partner
036200980629      *----------------------------------------------------------------
036300960528     C     WRST00        BEGSR
036400980629      *
036500980629      *  Pulisco dati DS
036600960528     C                   CLEAR                   EDST00
036700980629      *  Mittente: Bartolini  (ITBAR + AS..)
036800960528     C                   MOVEL     'ITBAR'       WMITT             8
036900960917     C                   MOVE      '005'         WMITT             8
037000960530     C                   MOVEL     WMITT         TA0004
037100960729     C                   MOVEL     'ZZ'          TA0007
037200980220      *
037300980629      *  In base al codice del PARTNER reperisco identificativo
037400980629     C                   MOVEL     CPT(XX)       TA0010
037500980629      *
037600980220      *  Utilizzo qualificatore x destinatario msg.
037700960827     C                   MOVEL     §PTMSQ        TAA007
037800980220      *  Data/Ora invio messagio: adesso
037900971031     C                   MOVEL     WOGGI         TA0017
038000960528     C                   MOVEL     WHHMM         TA0019
038100980220      *  Codice documento - nome documento
038200960528     C                   MOVEL     '784'         TA1001
038300960913     C                   MOVEL     '      '      TA1000
038400960708     C                   MOVEL     WHHDAT        TA1004                         DATA/ORA -> MSG
038500960528     C                   MOVEL     '9  '         TA1225
038600960528     C                   MOVEL     '137'         TA2005
038700980220      *  Data messaggio: oggi
038800960528     C                   MOVEL     WOGGI         TA2380
038900960528     C                   MOVEL     '102'         TA2379
039000100512      *
039100100512      *    Se si vuole la data con l'orario
039200100512     c                   if        §pudate203 ='S'
039300100512     C                   MOVE      Whhmm         TA2380
039400100512     C                   MOVEL(p)  '203'         TA2379
039500100512     c                   end
039600100512      *
039700980220      *  Scivo record IFTSTA
039800960705     C                   CLEAR                   WDAT
039900960705     C                   MOVEL     EDST00        WDAT
040000960726     C                   Z-ADD     §MSNUM        STAPRG
040100960726     C                   MOVEL     'ST00'        STATPR
040200960705     C                   MOVEL     WDAT          STADAT
040300960528     C                   WRITE     IFTSTA00
040400980220      *
040500960528     C                   ENDSR
040600980220      *----------------------------------------------------------------
040700980220      *? WRST05 - Scrittura record partner
040800980220      *----------------------------------------------------------------
040900960708     C     WRST05        BEGSR
041000980220      *
041100960708     C                   CLEAR                   EDST05
041200980220      *  Ship from:
041300960712     C                   MOVEL     *BLANKS       WDAT
041400960708     C                   MOVEL     'SF '         TE3035
041500970221     C                   MOVEL     §PTPLT        TE3036
041600980220      *  Scivo record IFCSUM
041700960708     C                   CLEAR                   WDAT
041800960708     C                   MOVEL     EDST05        WDAT
041900960726     C                   Z-ADD     §MSNUM        STAPRG
042000960712     C                   MOVEL     'ST05'        STATPR
042100960708     C                   MOVEL     WDAT          STADAT
042200960708     C                   WRITE     IFTSTA00
042300980220      *  Ship to:
042400960712     C                   MOVEL     *BLANKS       WDAT
042500960708     C                   CLEAR                   EDST05
042600960708     C                   MOVEL     'ST '         TE3035
042700970221     C                   MOVEL     COST1         TE3036
042800060503      *
042900060503      *  x CALBERSON --> GEODIS
043000060503      *      imposta la Platform che ci hanno chiesto di utilizzare
043100060503      *      x identificare diversamente dai dati di DUNLOP altrimenti
043200060503      *      non riescono a dividerli nel loro sistema.
043300060503      *  da impostare il §PTLOC ??????
043400060503     c                   if        §PTKSC = 493298
043500060503     C                   clear                   TE3036
043600060503     C                   MOVEL     CALB_Plat     TE3036
043700060503     c                   end
043800060503      *
043900980220      *  Scivo record IFCSUM
044000960708     C                   CLEAR                   WDAT
044100960708     C                   MOVEL     EDST05        WDAT
044200960726     C                   Z-ADD     §MSNUM        STAPRG
044300960712     C                   MOVEL     'ST05'        STATPR
044400960708     C                   MOVEL     WDAT          STADAT
044500960708     C                   WRITE     IFTSTA00
044600980220      *
044700960708     C                   ENDSR
044800980220      *----------------------------------------------------------------
044900980220      *? WRST10 - Scrittura record partner
045000980220      *----------------------------------------------------------------
045100960528     C     WRST10        BEGSR
045200980220      *
045300980220      *  Pulisco dati DS
045400960712     C                   MOVEL     *BLANKS       WDAT
045500960528     C                   CLEAR                   EDST10
045600980220      *  Se devo ritornare imposto AFB + nr.messaggio
045700960726     C                   MOVEL     'AFB'         TB1153
045800960726     C                   MOVEL     §MSNUM        TB1154
045900980220      *  Scivo record IFCSUM
046000960726     C                   CLEAR                   WDAT
046100960726     C                   MOVEL     EDST10        WDAT
046200960726     C                   Z-ADD     §MSNUM        STAPRG
046300960726     C                   MOVEL     'ST10'        STATPR
046400960726     C                   MOVEL     WDAT          STADAT
046500960726     C                   WRITE     IFTSTA00
046600980220      *
046700960528     C                   ENDSR
046800980220      *----------------------------------------------------------------
046900980220      *? WRST15 - Scrittura record partner
047000980220      *----------------------------------------------------------------
047100960528     C     WRST15        BEGSR
047200980220      *
047300980220      *  Pulisco dati DS
047400960712     C                   MOVEL     *BLANKS       WDAT
047500960528     C                   CLEAR                   EDST15
047600980220      *
047700960528     C                   ENDSR
047800980220      *----------------------------------------------------------------
047900980220      *? WRSD00 - Scrittura record partner - dettaglio
048000980220      *----------------------------------------------------------------
048100960528     C     WRSD00        BEGSR
048200980220      *
048300980220      *  Pulisco dati DS
048400960712     C                   MOVEL     *BLANKS       WDAT
048500960528     C                   CLEAR                   EDSD00
048600980220      *  Controllo se la bolla figlia con bolla originale su stesso AS
048700980220      *  non invio le date di consegna
048800970319     C                   Z-ADD     VACAAS        KAAN
048900970319     C                   Z-ADD     VACLNP        KLPN
049000970319     C                   Z-ADD     VACNRS        KNRN
049100970319     C                   Z-ADD     VACNSP        KNSN
049200970319     C     KLBL1         CHAIN     FNLBL01L                           31
049300970319     C     *IN31         IFEQ      '0'
049400030926     C***  LBLLPO        LOOKUP    £1                                     31
049500030926     C     LBLLPO        LOOKUP    Pou                                    31
049600970319     C   31              GOTO      NEWREC
049700970319     C                   END
049800980220      *
049900980220      *  Imposto numero spedizione originale cliente/partner
050000980220      *
050100961022     C                   MOVEL     *BLANKS       WNRSPE           35
050200960528     C                   Z-ADD     VACAAS        KAAS
050300960528     C                   Z-ADD     VACLNP        KLNP
050400960528     C                   Z-ADD     VACNRS        KNRS
050500960528     C                   Z-ADD     VACNSP        KNSP
050600040305      * La regola è:
050700040305      *  si deve restituire il Nostro Nr.spedizione nel CNI invece
050800060213      *   dal ar4 --> deve essere impostato l'RFF+AGE ossia Rif.Partner
050900080418      *
051000080418     c                   eval      cni_VACAAS = VACAAS
051100080418     c                   eval      cni_VACLNP = VACLNP
051200080418     c                   eval      cni_VACNRS = VACNRS
051300080418     c                   eval      cni_VACNSP = VACNSP
051400080418     c                   eval      cni_VACLNA = VACLNA
051500100209      *
051600100209     C********           MOVEL     CNI_spe       DA1004
051700040305      *
051800040305      * quindi occorre impostare diversamente i campi della DS00 e DS05
051900040305      *
052000960708     C                   MOVEL     'E'           KTRC
052100060213     C     Kar4          CHAIN     fiar401L                           31
052200980220      *
052300100722      * se non richiesta forzatura del Riferimento numerico da restituire
052400100722     c                   if        *in31 = '0' and §ptSGN =' '
052500100722     C**** *IN31         IFEQ      '0'
052600060213     C*****              MOVEL     ar4NOT        DA1004
052700060213     C                   MOVEL     ar4NOT        Wnrspe
052800960821     C                   ELSE
052900980220      *
053000060213      * Se non trovo ar4NOT utilizzo in cascata Rif.Mittente Alfa e numerico
053100980220      * (????) Segnalo se manca ricezione EDI e sped. bollettata a mano
053200980220      *
053300980220     C                   MOVEL     'S'           WSD15             1
053400960821     C     KBLP          CHAIN     FNBLP01L                           31
053500960821     C     *IN31         IFEQ      '0'
053600100722      * se non richiesta forzatura del Riferimento numerico da restituire
053700960821     C     BLPRMA        IFNE      *BLANKS
053800100722     C     §ptsgn        andeq     *BLANKS
053900040305     C******             MOVEL     BLPRMA        DA1004
054000040305     C                   MOVEL     BLPRMA        Wnrspe
054100960821     C                   ELSE
054200040305     C******             MOVEL     BLPRMN        DA1004
054300040305     C                   MOVEL     BLPRMN        Wnrspe
054400961007     C                   ENDIF
054500100304      **********
054600980220      * Se non ho impostato DA1004 leggo rcd successivo
054700100304     C**** DA1004        IFEQ      *BLANKS
054800100304     C**********         GOTO      NEWREC
054900100304     C**********         ENDIF
055000100304      **********
055100960821     C                   ENDIF
055200960708     C                   END
055300980220      *
055400040305     C*********          MOVEL     DA1004        WNRSPE
055500100209      *
055600100209      * Default
055700100209      *  se non richiesta l'inversione del dato CNI nell'AGE
055800100209     c                   if        §ptTRD <> 'S'
055900100209     C                   MOVEL(p)  CNI_spe       DA1004
056000100209      *  inverte con l'AGE
056100100209     c                   elseIF    §ptTRD  = 'S'
056200100209     C                   MOVEL(p)  Wnrspe        DA1004
056300100209      *
056400100209     c                   end
056500100304      *
056600100304      * Se non ho impostato DA1004 leggo rcd successivo
056700100304     C     DA1004        IFEQ      *BLANKS
056800100304     C                   GOTO      NEWREC
056900100304     C                   ENDIF
057000980220      *
057100960528     C                   ENDSR
057200980220      *----------------------------------------------------------------
057300980220      *? WRSD05 - Scrittura record partner - dettaglio
057400980220      *----------------------------------------------------------------
057500960528     C     WRSD05        BEGSR
057600980220      *
057700980220      *  Pulisco dati DS
057800960712     C                   MOVEL     *BLANKS       WDAT
057900960528     C                   CLEAR                   EDSD05
058000980220      *
058100980220      *  Se ho l'obbligo di ritornaglielo scrivo nr. CMR altrimenti nr. sped.
058200980220     C     §PURFF        IFEQ      'C'
058300980220     C                   EXSR      NUMCMR
058400980220     C                   ELSE
058500961022     C                   MOVEL     'AGE'         DB1153
058600100209      *
058700100209      * Default
058800100209      *  se non richiesta l'inversione del dato CNI nell'AGE
058900100209     c                   if        §ptTRD <> 'S'
059000961022     C                   MOVEL     WNRSPE        DB1154
059100100209      *  inverte con il CNI
059200100209     c                   elseIF    §ptTRD  = 'S'
059300100209     C                   MOVEL     CNI_spe       DB1154
059400100209      *
059500100209     c                   end
059600100209      *
059700980220     C                   END
059800980220      *
059900991019      * Per Europolitan imposto numero spedizione dalle pos.16-35
060000080418     C**** TPR           IFEQ      'S'
060100080418     C**********         MOVE      DATSPE        DB1154
060200080418     C**********         END
060300980220      *------------------------------------------------------*
060400980220      *  Se la bolla è una mamma controllo se è stato effettuato un evento IDD
060500970319     C                   MOVEL     'N'           WTREVE            1
060600970319     C                   Z-ADD     VACAAS        KAAP
060700970319     C                   Z-ADD     VACLNP        KLPP
060800970319     C                   Z-ADD     VACNRS        KNRP
060900970319     C                   Z-ADD     VACNSP        KNSP
061000970319     C     KLBL2         CHAIN     FNLBL02L                           31
061100980220      *
061200980220      *  Scorro schiera eventi IDD e controllo se ce ne sono
061300980220     C     *IN31         IFEQ      '0'
061400970319     C                   Z-ADD     1             X
061500980220      *
061600980220     C     IDD(X)        DOWNE     *BLANKS
061700970319     C     X             ANDLE     200
061800970319     C     WTREVE        ANDEQ     'N'
061900970319     C                   MOVEL     IDD(X)        KCEV
062000970319     C                   Z-ADD     VACAAS        KAA2
062100970319     C                   Z-ADD     VACLNP        KLNP
062200970319     C                   Z-ADD     VACNRS        KNRS
062300970319     C                   Z-ADD     VACNSP        KNSP
062400990810     C     KEVB          CHAIN     FNEVB04L                           31
062500980220     C  N31              MOVEL     'S'           WTREVE
062600980220     C   31              ADD       1             X
062700980220     C                   END
062800980220      *
062900980220     C                   END
063000980220      *------------------------------------------------------*
063100990604      *  APERTURA C.A.
063200990604     C                   MOVEL     *BLANKS       WELACA            1
063300990604     C     VACCCA        IFEQ      'A'                                          -- 01 --
063400990604      *
063500990604      *  Controllo se ho C.A. aperte non trasmesse
063600040715     C**** KBLP          CHAIN     FNDCT000                           31
063700040715      ****
063800040715     C**** *IN31         DOWEQ     *OFF                                         -- 02 --
063900040715     C****               MOVEL     DCTFLO        DDCT01
064000040715     C**** DCTATB        IFEQ      *BLANKS
064100040715     C**** DCTDCH        ANDEQ     *ZEROS
064200040715     C**** §DCTtrpt      ANDEQ     *BLANKS
064300040715     C****               MOVEL     'S'           WELACA
064400040715     C****               SETON                                        31
064500040715     C****               ELSE
064600040715     C**** KBLP          READE     FNDCT000                               31
064700040715     C****               ENDIF
064800040715     C****               ENDDO                                                  -- 02 --
064900040715      *
065000040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
065100040715      * SIA COME MAMMA CHE COME FIGLIA
065200040715     c                   clear                   fidn12ds
065300040715     c                   eval      i12aas = kAAS
065400040715     c                   eval      i12lnp = kLNP
065500040715     c                   eval      i12nrs = kNRS
065600040715     c                   eval      i12nsp = kNSP
065700040715     c                   eval      i12fel = 'E'
065800040715     c                   eval      i12fan = 'E'
065900040715     c                   eval      i12fch = 'E'
066000040715     c                   eval      i12fpt = 'E'
066100040715     c                   call      'FIDN12R'
066200040715     c                   parm                    fidn12ds
066300040715     c                   z-add     *zeros        ii                2 0
066400040715      **
066500040715      * se trovata almeno una CA
066600040715     c                   if        o12nca > 0
066700040715     c                   dow       ii <  o12nca
066800040715      **
066900040715     c                   add       1             ii
067000040715     c                   movea     skkey(ii)     dskey
067100040715     C                   z-add     dsaac         kaac
067200040715     C                   z-add     dsfil         kfil
067300040715     C                   z-add     dsnca         knca
067400040715     c     kdct          chain(n)  FNDCT000
067500040715     c                   If        %found(fndct01l)
067600040715     C                   MOVEL     DCTFLO        DDCT01
067700040715     C     §DCTtrpt      ifeq      *BLANKS
067800040715     C                   MOVEL     'S'           WELACA
067900040715     C                   leave
068000040715     C                   endif
068100040715     C                   endif
068200040715      *
068300040715     c                   enddo
068400040715     c                   endif
068500990604      *
068600990604     C     WELACA        IFEQ      'S'                                          -- 02 --
068700990604      *  Determino la causale evento e controllo se reso codificato in tabella
068800991019      *   IDD per mancanze EuroExpress ed Europolitan; RFD per Avarie e Ammanchi
068900991019      *    EuroExpress; per Avarie e Ammanchi Europolitan non trasmetto nulla.
069000990604     C                   Z-ADD     1             X
069100991019     C                   SETOFF                                       32
069200990604      *
069300991019     C                   SELECT
069400080418     C**** TPR           WHENEQ    'S'                                          Europolitan
069500080418     C**** DCTTAD        ANDNE     '01'                                            no
069600080418     C**** DCTTAD        ANDNE     '21'                                          mancanze
069700991019      *
069800080418     C**** TPR           WHENEQ    'S'                                          Europolitan
069900080418     C**** 'IDD'         LOOKUP    C2A(X)                                 32
070000991019      *
070100991019     C     DCTTAD        WHENNE    '01'                                         EuroExpress
070200991019     C     DCTTAD        ANDNE     '21'                                         mancanze
070300990604     C     'RFD'         LOOKUP    C2A(X)                                 32
070400991019      *
070500991019     C                   OTHER                                                  EuroExpress altro
070600990604     C     'IDD'         LOOKUP    C2A(X)                                 32
070700991019     C                   ENDSL
070800990604      *
070900990604     C     *IN32         IFEQ      '1'                                          -- 03 --
071000990604     C     §PULAB        IFEQ      'S'
071100990604     C                   MOVEL     CEV(X)        DB9013
071200990604     C                   MOVEL     STS(X)        DB9011
071300990604     C                   ELSE
071400990604     C                   MOVEL     CDV(X)        DB9013
071500990604     C                   MOVEL     STV(X)        DB9011
071600990604     C                   ENDIF
071700990604     C                   MOVEL     '7  '         DB2005
071800990604     C                   MOVEL     VACDCM        WDATHM
071900990604     C                   MOVE      VACHMC        WDATHM           12 0
072000990604     C                   MOVEL     WDATHM        DB2380
072100990604     C                   MOVEL     '203'         DB2379
072200990604      *  Scrivo record SD00 e SD05
072300990604     C                   EXSR      WRT0E5
072400990604     C                   ENDIF                                                  -- 03 --
072500990604      *
072600990604     C                   ENDIF                                                  -- 02 --
072700990608     C                   GOTO      FIND05
072800990604     C                   ENDIF                                                  -- 01 --
072900990604      *------------------------------------------------------*
073000980220      *  Controllo se è stata effettuata un consegna parziale
073100970319     C                   Z-ADD     VACAAS        KAAS
073200970319     C                   Z-ADD     VACLNP        KLNP
073300970319     C                   Z-ADD     VACNRS        KNRS
073400970319     C                   Z-ADD     VACNSP        KNSP
073500970319     C     KBLP          CHAIN     FNBLP01L                           31
073600980220      *
073700980220     C     BLPDCP        IFGT      0
073800960729     C     BLPDCM        ANDEQ     0
073900980220      *
074000980220      *  Controllo se devo trasmettere i colli
074100980220     C     WTREVE        IFEQ      'S'
074200970320     C                   EXSR      CHKBLT
074300980220      *  Se  non ci sono colli da trasmettere passo al record successivo
074400980220     C     WTRCOL        IFEQ      'N'
074500980220     C                   GOTO      NEWREC
074600980220     C                   END
074700980220     C                   END
074800980220      *
074900960528     C                   Z-ADD     1             X
075000970617     C                   SETOFF                                       32
075100980220      *
075200970617     C     §PTDET        IFEQ      'M'
075300970320     C     BLPCCA        ANDEQ     '5'
075400970320     C     §PTDET        OREQ      'M'
075500970320     C     WTREVE        ANDEQ     'S'
075600970320     C     'PID'         LOOKUP    C2A(X)                                 32
075700970320     C                   ELSE
075800970320     C     'P  '         LOOKUP    C2A(X)                                 32
075900970320     C                   END
076000980220      *
076100980220     C   32§PULAB        IFEQ      'S'
076200960528     C                   MOVEL     CEV(X)        DB9013
076300960612     C                   MOVEL     STS(X)        DB9011
076400961022     C                   ELSE
076500961022     C                   MOVEL     CDV(X)        DB9013
076600961022     C                   MOVEL     STV(X)        DB9011
076700980220     C                   END
076800980220      *
076900960528     C                   MOVEL     '7  '         DB2005
077000960528     C                   MOVEL     BLPDCP        DB2380
077100960528     C                   MOVEL     '102'         DB2379
077200100512      *
077300100512      *    Se si vuole la data con l'orario
077400100512     c                   if        §pudate203 ='S'
077500100512     C                   MOVE      '0001'        TA2380
077600100512     C                   MOVEL(p)  '203'         TA2379
077700100512     c                   end
077800100512      *
077900980220      *  Scrivo record SD00 e SD05
078000980220     C                   EXSR      WRT0E5
078100960528     C                   GOTO      FIND05
078200980220     C                   END
078300990604      *-------------------------------------------------------------*
078400050922      * aggancia sempre la giacenza se c'è stata giacenza e non è consegnata
078500050922     C     vacDAG        ifGT      0                                             -- 0A --
078600050922     C     vacDCM        andEQ     0
078700050922     C                   Z-ADD     VACAAS        KAAS
078800050922     C                   Z-ADD     VACLNP        KLNP
078900050922     C                   Z-ADD     VACNRS        KNRS
079000050922     C                   Z-ADD     VACNSP        KNSP
079100050922     c                   Z-ADD     0             KFRG
079200050922     C     KGCP          CHAIN     tigcp01L                           31
079300050922     c                   if        %Found(tigcp01L)                              -- 0B --
079400050922
079500050922      * imposta la data Distinta/udate apertura giacenza fuori distinta
079600050922     c                   z-add     GCPDXD        vacDAG
079700050922
079800980220      *  Se la data più alta è di giacenza invio data apertura giacenza
079900970617     C     VACDAG        IFGT      VACDLA                                       -- 01 --
080000960528     C     VACDAG        ANDGT     VACDCM
080100960528     C     VACDAG        ANDGT     0
080200050922     C****               Z-ADD     0             KFRG
080300050922     C***  KGCP          CHAIN     tigcp01L                           31
080400980220      *  Controllo se il codice apertura giacenza è codificato in tabella
080500960528     C                   Z-ADD     1             X
080600960528     C     GCPCMC        LOOKUP    C2A(X)                                 32
080700980220      *
080800970617     C     *IN32         IFEQ      '1'                                          -- 02 --
080900980220      *
081000980220     C     §PULAB        IFEQ      'S'                                          -- 03 --
081100960528     C                   MOVEL     CEV(X)        DB9013
081200960612     C                   MOVEL     STS(X)        DB9011
081300980220     C                   ELSE                                                   -- 03 --
081400961022     C                   MOVEL     CDV(X)        DB9013
081500961022     C                   MOVEL     STV(X)        DB9011
081600980220     C                   END                                                    -- 03 --
081700980220      *
081800960528     C                   MOVEL     '7  '         DB2005
081900050909      * ----->
082000050919      *   Imposta la Data e le ore 18:00 fisse. Prima prendeva sempre la data
082100050919      *   apertura/riapertura della giacenza invece,
082200050919      *    questa è la data della distinta di consegna da ARB oppure
082300050919      *    la UDATE se si è aperta giacenza non passando da distinta.
082400050919     C                   movel     gcpDXD        WDATHM
082500050909     C                   move      '1800'        WDATHM
082600050909     C                   movel     WDATHM        DB2380
082700050909      *
082800041201      * attenzione se si tratta di una riapertura giacenza deve comunicare
082900041201      *  la data di ultima riapertura assieme al motivo di giacenza.
083000050919     c*********          if        gcpDUR > vacDAG
083100050919      *********
083200050919     C*********          z-add     gcpDUR        data08            8 0
083300050919     C*********          MOVEL     data08        DB2380
083400050919     C*********          movel     data08        WDATHM
083500050919     C*********          move      '1800'        WDATHM
083600050919     C*********          movel     WDATHM        DB2380
083700050919      *********
083800050919     c*********          else
083900050919      *********
084000050919     C*********          MOVEL     VACDAG        DB2380
084100050919     C*********          movel     vacdag        WDATHM
084200050919     C*********          move      '1800'        WDATHM
084300050919     C*********          movel     WDATHM        DB2380
084400050919      *********
084500050919     c*********          end
084600050919      *********
084700050922      *
084800050922      * Controlla se è stato fatto un evento di giacenza senza aver
084900050922      *  successivamente riaperto Giacenza ... prende questo come Data/Ora
085000050922     C     KGCP_1        setgt     fnEVB04L
085100050922     C     KGCP_1        readpe    fnEVB04L
085200050922     c                   dow       not %EoF(fnEVB04L)
085300050922     c     evbCEV        lookup    GiaC                                   31
085400050922     c                   if        *in31 = *on
085500050922     c                   if        evbDEV > gcpDXD
085600050922     C                   movel     evbDEV        WDATHM
085700050922     C                   move      evbHEV        WDATHM
085800050922     C                   movel     WDATHM        DB2380
085900050922     c                   end
086000050922     c                   leave
086100050922     c                   end
086200050922     C     KGCP_1        readpe    fnEVB04L
086300050922     c                   enddo
086400050922      *********
086500050919     C*********          MOVEL     '102'         DB2379
086600050909     C                   movel     '203'         DB2379
086700050922      *
086800980220      *  Scrivo record SD00 e SD05
086900980220     C                   EXSR      WRT0E5
087000980220     C                   GOTO      FIND05
087100970617     C                   END                                                    -- 02 --
087200970617     C                   END                                                    -- 01 --
087300050922
087400050922     c                   end                                                    -- 0B --
087500050922     c                   end                                                    -- 0A --
087600980220      *-------------------------------------------------------------*
087700980220      *  La spedizione ha avuto lasciato avviso
087800980220     C     VACDLA        IFGT      VACDCM
087900960528     C     VACDLA        ANDGT     0
088000980220     C*  Controllo se il codice lasciato avviso è codificato in tabella
088100960528     C                   Z-ADD     1             X
088200960528     C     'AVV'         LOOKUP    C2A(X)                                 32
088300980220      *
088400960528     C     *IN32         IFEQ      '1'
088500980220      *
088600961022     C     §PULAB        IFEQ      'S'
088700960528     C                   MOVEL     CEV(X)        DB9013
088800960612     C                   MOVEL     STS(X)        DB9011
088900961022     C                   ELSE
089000961022     C                   MOVEL     CDV(X)        DB9013
089100961022     C                   MOVEL     STV(X)        DB9011
089200961022     C                   END
089300980220      *
089400960528     C                   MOVEL     '7  '         DB2005
089500050926     C******             MOVEL     VACDLA        DB2380
089600050926     C******             MOVEL     '102'         DB2379
089700050926     C                   movel     VACDLA        WDATHM
089800050926     C                   move      1800          WDATHM
089900050926     C                   movel     WDATHM        DB2380
090000050926     C                   MOVEL     '203'         DB2379
090100050922      *
090200050922      * Controlla se è stato fatto un evento di L.AVV. posteriore
090300050922      *  a quello presente nel vacDLA ... prende questo come Data/Ora
090400050922     C                   Z-ADD     VACAAS        KAAS
090500050922     C                   Z-ADD     VACLNP        KLNP
090600050922     C                   Z-ADD     VACNRS        KNRS
090700050922     C                   Z-ADD     VACNSP        KNSP
090800050922     C     KGCP_1        setgt     fnEVB04L
090900050922     C     KGCP_1        readpe    fnEVB04L
091000050922     c                   dow       not %EoF(fnEVB04L)
091100050926     c     evbCEV        lookup    LAvv                                   31
091200050922     c                   if        *in31 = *on
091300050922     c                   if        evbDEV >= vacDLA
091400050922     C                   movel     evbDEV        WDATHM
091500050922     C                   move      evbHEV        WDATHM
091600050922     C                   movel     WDATHM        DB2380
091700050922     C                   MOVEL     '203'         DB2379
091800050922     c                   end
091900050922     c                   leave
092000050922     c                   end
092100050922     C     KGCP_1        readpe    fnEVB04L
092200050922     c                   enddo
092300050922
092400980220      *  Scrivo record SD00 e SD05
092500980220     C                   EXSR      WRT0E5
092600980220     C                   GOTO      FIND05
092700960528     C                   END
092800970617     C                   END                                                    -- 01 --
092900980220      *-------------------------------------------------------------*
093000081117      *
093100081117      *  -> STATUS 20/210 Appuntamento (SUPERMERCATO)
093200081117      * Non è un evento ma una particolarità di consegna
093300081117      *  aggiunto anche per appuntamento (Booking ins)
093400081117     c                   eval      Bolla_variata  = 'N'
093500081117      *
093600081117     c                   if        VACTC1 = 'S' or VACTC2 = 'S' or
093700081117     c                             VACTC1 = 'A' or VACTC2 = 'A'
093800081117     c                   Exsr      STATUS_20_210
093900081117     c                   end
094000081117      *
094100081117      *-------------------------------------------------------------*
094200980220      *  La spedizione è stata chiusa per: RESO - IDD - CONSEGNA
094300970617     C     VACDCM        IFGT      0                                            -- 01 --
094400970617     C     VACCCA        ANDNE     '7'
094500970617     C     VACCCA        ANDNE     'P'
094600970617     C     BLPDCM        ORGT      0
094700980220      *
094800980220      *---------------------------------*
094900980220      *  RESO
095000970617     C     VACCCA        IFEQ      '2'                                          -- 02 --
095100960712     C     VACCCA        OREQ      '6'
095200980220      *
095300980220      *  Controllo se reso codificato in tab.
095400960528     C                   Z-ADD     1             X
095500980220     C     'RES'         LOOKUP    C2A(X)                                 32      ??? RESO
095600970617     C     *IN32         IFEQ      '1'                                          -- 03 --
095700961022     C     §PULAB        IFEQ      'S'
095800960528     C                   MOVEL     CEV(X)        DB9013
095900960612     C                   MOVEL     STS(X)        DB9011
096000961022     C                   ELSE
096100961022     C                   MOVEL     CDV(X)        DB9013
096200961022     C                   MOVEL     STV(X)        DB9011
096300961022     C                   END
096400960528     C                   MOVEL     '7  '         DB2005
096500960528     C                   MOVEL     VACDCM        WDATHM
096600960528     C                   MOVE      VACHMC        WDATHM           12 0
096700960528     C                   MOVEL     WDATHM        DB2380
096800960528     C                   MOVEL     '203'         DB2379
096900980220      *  Scrivo record SD00 e SD05
097000980220     C                   EXSR      WRT0E5
097100980220     C                   GOTO      FIND05
097200970617     C                   END                                                    -- 03 --
097300980220      *---------------------------------*
097400970617     C                   ELSE                                                   -- 02 --
097500980220      *---------------------------------*
097600980220      *  COLLI MANCANTI - IDD -
097700980220      *
097800970617     C     VACCCA        IFEQ      '5'                                          -- 03 --
097900970617     C     VACCCA        OREQ      '3'
098000970617     C     VACCCA        OREQ      '7'
098100970319     C     WTREVE        OREQ      'S'
098200980220      *
098300980220      *  Controllo se reso codificato in tab.
098400970121     C                   Z-ADD     1             X
098500970121     C     'IDD'         LOOKUP    C2A(X)                                 32     IDD
098600980220      *
098700970617     C     *IN32         IFEQ      '1'                                          -- 04 --
098800970121     C     §PULAB        IFEQ      'S'
098900970121     C                   MOVEL     CEV(X)        DB9013
099000970121     C                   MOVEL     STS(X)        DB9011
099100970121     C                   ELSE
099200970121     C                   MOVEL     CDV(X)        DB9013
099300970121     C                   MOVEL     STV(X)        DB9011
099400970121     C                   END
099500970121     C                   MOVEL     '7  '         DB2005
099600970121     C                   MOVEL     VACDCM        WDATHM
099700970121     C                   MOVE      VACHMC        WDATHM           12 0
099800970121     C                   MOVEL     WDATHM        DB2380
099900970121     C                   MOVEL     '203'         DB2379
100000980220      *  Scrivo record SD00 e SD05
100100980220     C                   EXSR      WRT0E5
100200980220     C                   GOTO      FIND05
100300970617     C                   END                                                    -- 04 --
100400980220      *---------------------------------*
100500970617     C                   ELSE                                                   -- 03 --
100600980319      *---------------------------------*
100700980319      * Se VACCCA='C' --> ho ricevuto dall'arrivo e devo trasmettere
100800980319      *                   al partner un evento di messa in consegna
100900980319     C     VACCCA        IFEQ      'C'                                          -- 04 --
101000040707      *---------------------------------*
101100040707      *  Prima del MIC        -> STATUS 20/210 SUPERMERCATO
101200040707      *
101300040707      * Non è un evento ma una prticolarità di consegna
101400040721      *  aggiunto anche per appuntamento (Booking ins)
101500081111      *    SOLO SE NON E'STAT MANDATA PRIMA:
101600081112     c                   if        Bolla_variata  = 'N'
101700081112      *
101800081111     c                   if        VACTC1 = 'S' or VACTC2 = 'S' or
101900081111     c                             VACTC1 = 'A' or VACTC2 = 'A'
102000081111     c                   Exsr      STATUS_20_210
102100081111     c                   end
102200081119      *
102300081111     c                   endif
102400081111     c**
102500040707      *---------------------------------*
102600980319     C                   Z-ADD     1             X
102700980319     C     'MIC'         LOOKUP    C2A(X)                                 32
102800980319      *
102900980319     C     *IN32         IFEQ      '1'                                          -- 05 --
103000980319     C     §PULAB        IFEQ      'S'
103100980319     C                   MOVEL     CEV(X)        DB9013
103200980319     C                   MOVEL     STS(X)        DB9011
103300980319     C                   ELSE
103400980319     C                   MOVEL     CDV(X)        DB9013
103500980319     C                   MOVEL     STV(X)        DB9011
103600980319     C                   END
103700980319     C                   MOVEL     '7  '         DB2005
103800980319     C                   MOVEL     VACDCM        WDATHM
103900980319     C                   MOVE      VACHMC        WDATHM           12 0
104000980319     C                   MOVEL     WDATHM        DB2380
104100980319     C                   MOVEL     '203'         DB2379
104200980319      *  Scrivo record SD00 e SD05
104300980319     C                   EXSR      WRT0E5
104400980319     C                   GOTO      FIND05
104500980319     C                   END                                                    -- 05 --
104600980319     C                   ELSE                                                   -- 04 --
104700980220      *---------------------------------*
104800980220      *  CONSEGNA TOTALE
104900970617     C                   Z-ADD     1             X
105000960528     C     'CON'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
105100980220      *
105200980319     C     *IN32         IFEQ      '1'                                          -- 05 --
105300961022     C     §PULAB        IFEQ      'S'
105400961022     C                   MOVEL     CEV(X)        DB9013
105500961022     C                   MOVEL     STS(X)        DB9011
105600961022     C                   ELSE
105700961022     C                   MOVEL     CDV(X)        DB9013
105800961022     C                   MOVEL     STV(X)        DB9011
105900961022     C                   END
106000960528     C                   MOVEL     '7  '         DB2005
106100960528     C                   MOVEL     VACDCM        WDATHM
106200960528     C                   MOVE      VACHMC        WDATHM           12 0
106300960528     C                   MOVEL     WDATHM        DB2380
106400960528     C                   MOVEL     '203'         DB2379
106500980220      *  Scrivo record SD00 e SD05
106600980220     C                   EXSR      WRT0E5
106700980220     C                   GOTO      FIND05
106800980319     C                   END                                                    -- 05 --
106900980220      *
107000980220      *---------------------------------*
107100980319     C                   END                                                    -- 04 --
107200980319     C                   END                                                    -- 03 --
107300970617     C                   END                                                    -- 02 --
107400980220      *---------------------------------*
107500980220      *  La spedizione è stata chiusa per: IDD PARTENZA
107600980220     C                   ELSE
107700980220      *---------------------------------*
107800980220      *
107900980220      * Chiusura bolla x IDD in partenza
108000970617     C     VACCCA        IFEQ      '7'                                          -- 02 --
108100970617     C     VACCCA        OREQ      'P'                                          -- 02 --
108200970617     C                   Z-ADD     1             X
108300980204     C                   Z-ADD     VACAAS        KAA2
108400980204     C                   Z-ADD     VACLNP        KLNP
108500980204     C                   Z-ADD     VACNRS        KNRS
108600980204     C                   Z-ADD     VACNSP        KNSP
108700990810     C                   Z-ADD     WOGGI         KDTV
108800980204     C                   TIME                    KORV
108900990810     C     KEVB1         SETGT     FNEVB05L
109000990810     C     KEVB2         READPE    FNEVB05L                               31
109100980220      *
109200980204     C     EVBCEV        LOOKUP    C2A(X)                                 32     IDD
109300980220      *
109400970617     C     *IN32         IFEQ      '1'                                          -- 03 --
109500970617     C     §PULAB        IFEQ      'S'
109600970617     C                   MOVEL     CEV(X)        DB9013
109700970617     C                   MOVEL     STS(X)        DB9011
109800970617     C                   ELSE
109900970617     C                   MOVEL     CDV(X)        DB9013
110000970617     C                   MOVEL     STV(X)        DB9011
110100970617     C                   END
110200970617     C                   MOVEL     '7  '         DB2005
110300970617     C                   MOVEL     VACDCM        WDATHM
110400970617     C                   MOVE      VACHMC        WDATHM           12 0
110500970617     C                   MOVEL     WDATHM        DB2380
110600970617     C                   MOVEL     '203'         DB2379
110700980220      *  Scrivo record SD00 e SD05
110800980220     C                   EXSR      WRT0E5
110900980220     C                   GOTO      FIND05
111000970617     C                   END                                                    -- 03 --
111100970617     C                   END                                                    -- 02 --
111200980220      *---------------------------------*
111300970617     C                   END
111400980220      *
111500960528     C     FIND05        ENDSR
111600040604      *-------------------------------------------------------------*
111700040604      *? STATUS_20_210      << SUPERMERCATO >>
111800040604      *----------------------------------------------------------------
111900040604     C     STATUS_20_210 Begsr
112000040604      *
112100131014     c                   setoff                                       14
112200081112     c                   eval      variata    = 'N'
112300081112      *
112400040607      * Si deve prendere l'ultima variazione del solo tipo record ARBG
112500040607      *  appartenente ad una causale CR, CP, CS, CD
112600040607     C                   Z-ADD     VACAAS        KAAS
112700040607     C                   Z-ADD     VACLNP        KLNP
112800040607     C                   Z-ADD     VACNRS        KNRS
112900040607     C                   Z-ADD     VACNSP        KNSP
113000131014     c     kARF          setgt     fiarbf2c
113100131014     c     kARF          readpe    fiarbf2c
113200131014     c                   dow       not %eof(fiarbf2c)
113300040607      *
113400040607      * solo x tipo record FNARBG00 (variazione bolla)
113500080922     c                   if        *in14 = *on
113600080922      *
113700081111      ** con causale particolare
113800040607     c                   if        arbCVB = 'CR' or
113900040607     c                             arbCVB = 'CS' or
114000080922     c                             arbCVB = 'CD'
114100081119      * Se presente una variazione
114200081119     c                   eval      Bolla_variata  = 'S'
114300080922      *
114400081111      *  Solo se la variazione è avvenuta in giornata viene inviata altrimenti non
114500081111      *   deve essere presa in considerazione:
114600081111     c                   if        arbDTV = wOggi
114700081112     c                   eval      variata        = 'S'
114800040607     c                   leave
114900081111     c                   endIf
115000081111      *
115100040607     c                   end
115200080922      *
115300040607     c                   end
115400080922      *
115500131014     c                   setoff                                       14
115600131014     c     kARF          readpe    fiarbf2c
115700040607     c                   enddo
115800040607      *
115900040607     c                   if        variata = 'S'
116000040607      * data e ora variazione
116100040607     C                   MOVEL     arbORV        Wora              4 0
116200040607     C                   MOVE      Wora          WDATHM           12 0
116300040607     C                   MOVEL     arbDTV        WDATHM
116400040607     c                   else
116500040607      * data spedizione
116600040607     C                   MOVEL     vacaas        Wdata             8 0
116700040607     C                   MOVE      vacmgs        Wdata
116800040607     C                   MOVE      '0800'        WDATHM           12 0
116900040607     C                   MOVEL     Wdata         WDATHM
117000040607     c                   end
117100040607      *
117200040607      *  20+210 --> segnala se consegnata a un Supermercato
117300040608     C                   MOVEL     '20 '         DB9011
117400040608     C                   MOVEL     '210'         DB9013
117500040607      *
117600040607     C                   MOVEL     '7  '         DB2005
117700040607     C                   MOVEL     WDATHM        DB2380
117800040607     C                   MOVEL     '203'         DB2379
117900040607      *
118000040607      *  Scrivo record SD00 e SD05 prima dei 2 records della consegna
118100040607     C                   EXSR      WRT0E5
118200040607      *
118300040607      * e nel FTX la data di consegna richiesta se presente
118400040607      *  Pulisco dati DS
118500040607     c                   if        vacdcr <> *zero
118600040607     C                   MOVEL     *BLANKS       WDAT
118700040607     C                   CLEAR                   EDSD15
118800040607      *
118900040607      *   Note generiche ='AAI'
119000040607     C                   MOVEL     'AAI'         DD4451
119100040607     c                   select
119200040607     c                   when      vactcr = *blank
119300040607     C                   eval      DD4440 = 'ON ' +
119400040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
119500040607     c                             %editw(vachcr:'  :  ')
119600040607     c                   when      vactcr = 'P'
119700040607     C                   eval      DD4440 = 'BEFORE ' +
119800040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
119900040607     c                             %editw(vachcr:'  :  ')
120000040607     c                   when      vactcr = 'D'
120100040607     C                   eval      DD4440 = 'AFTER ' +
120200040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
120300040607     c                             %editw(vachcr:'  :  ')
120400040607     c                   endsl
120500040607      *  Scrivo record dati note
120600040607     C                   MOVEL     EDSD15        WDAT
120700040607     C                   Z-ADD     §MSNUM        STAPRG
120800040607     C                   MOVEL     'SD15'        STATPR
120900040607     C                   MOVEL     WDAT          STADAT
121000040607     C                   WRITE     IFTSTA00
121100080422     c                   eval      Righe_Dett = 'S'
121200040607     C                   End
121300040607      *
121400040604     C                   Endsr
121500980220      *-------------------------------------------------------------*
121600980220      *? NUMCMR - Scrivo numero CMR
121700980220      *----------------------------------------------------------------
121800980220     C     NUMCMR        BEGSR
121900980220      *
122000980220     C                   MOVEL     '784'         DB1153
122100980220      *
122200980220     C     WTRRDE        IFEQ      'S'
122300980220      *
122400980220     C     §PTCM1        IFEQ      'S'
122500980220     C     §PTNF1        OREQ      'S'
122600980220     C                   Z-ADD     VACAAS        KAAS
122700980220     C                   Z-ADD     VACLNP        KLNP
122800980220     C                   Z-ADD     VACNRS        KNRS
122900980220     C                   Z-ADD     VACNSP        KNSP
123000980220     C                   MOVEL     'TD1154'      KNMC
123100980220     C                   Z-ADD     1             KNRP1
123200980220     C     KRDE          CHAIN     EDRDE01L                           31
123300980220     C  N31              MOVEL     RDEVAL        DB1154
123400980220     C                   END
123500980220      *
123600980220     C                   END
123700980220      *
123800980220     C                   ENDSR
123900980220      *-------------------------------------------------------------*
124000980220      *? WRT0E5 - Scrittura records d00 e d05
124100980220      *----------------------------------------------------------------
124200980220     C     WRT0E5        BEGSR
124300980220      *  Scrivo tipo record SD00
124400980220     C                   ADD       1             WPROG
124500980220     C                   MOVEL     WPROG         DA1490
124600980220     C                   MOVEL     '9999'        DA1312
124700980220     C                   CLEAR                   WDAT
124800980220     C                   MOVEL     EDSD00        WDAT
124900980220     C                   Z-ADD     §MSNUM        STAPRG
125000980220     C                   MOVEL     'SD00'        STATPR
125100980220     C                   MOVEL     WDAT          STADAT
125200980220     C                   WRITE     IFTSTA00
125300980220      *  Scrivo tipo record SD05
125400980220     C                   MOVEL     EDSD05        WDAT
125500980220     C                   Z-ADD     §MSNUM        STAPRG
125600980220     C                   MOVEL     'SD05'        STATPR
125700980220     C                   MOVEL     WDAT          STADAT
125800980220     C                   WRITE     IFTSTA00
125900980220     C                   MOVEL     'S'           WRTEVE            1
126000080422     c                   eval      Righe_Dett = 'S'
126100980220      *
126200980220     C                   ENDSR
126300980220      *-------------------------------------------------------------*
126400980220      *? WRSD10 - Scrittura record partner - dettaglio
126500980220      *----------------------------------------------------------------
126600980220     C     WRSD10        BEGSR
126700101028      *
126800101028      *  solo chi vuole ricevere la firma del firmatario
126900101028     C                   IF        §PUfirma <>'N'
127000980220      *
127100960712     C                   MOVEL     *BLANKS       WDAT
127200960528     C                   CLEAR                   EDSD10
127300101028      *
127400101028      *  Per Schneider questo segmento NON DEVE essere inviato
127500101028      *
127600980220      *  Controllo se esiste firmatario
127700960528     C                   MOVEL     '1'           KTRC
127800060213     C     Kar4          CHAIN     fiar401L                           31
127900960528     C     *IN31         IFEQ      '0'
128000960528     C                   MOVEL     'AP '         DC3035
128100060213     C                   MOVEL     ar4NOT        DC3036
128200980220      *  Scrivo record dati firmatario
128300960705     C                   MOVEL     EDSD10        WDAT
128400960729     C                   Z-ADD     §MSNUM        STAPRG
128500960705     C                   MOVEL     'SD10'        STATPR
128600960705     C                   MOVEL     WDAT          STADAT
128700960528     C                   WRITE     IFTSTA00
128800080422     c                   eval      Righe_Dett = 'S'
128900960528     C                   END
129000980220      *
129100101028     c                   end
129200101028      *
129300960528     C                   ENDSR
129400980220      *-------------------------------------------------------------*
129500980220      *? WRSD15 - Scrittura record partner - dettaglio
129600980220      *----------------------------------------------------------------
129700960528     C     WRSD15        BEGSR
129800980220      *
129900980220      *  Pulisco dati DS
130000960712     C                   MOVEL     *BLANKS       WDAT
130100960528     C                   CLEAR                   EDSD15
130200980220      *
130300980220      *  Controllo se ci sono delle note di consegna
130400980220     C     VACDCM        IFNE      0                                            -- 01 --
130500980204     C                   MOVEL     *BLANKS       RDEVAL
130600980220      *
130700980220     C     WTRRDE        IFEQ      'S'                                          -- 02 --
130800980204     C                   Z-ADD     VACAAS        KAAS
130900980204     C                   Z-ADD     VACLNP        KLNP
131000980204     C                   Z-ADD     VACNRS        KNRS
131100980204     C                   Z-ADD     VACNSP        KNSP
131200980204     C                   MOVEL     'DD4440'      KNMC
131300980204     C                   Z-ADD     1             KNRP1
131400980204     C     KRDE          CHAIN     EDRDE01L                           31
131500980220      *
131600980220     C     *IN31         IFEQ      '0'                                          -- 03 --
131700980204     C                   MOVEL     'AAI'         DD4451
131800980204     C                   MOVEL     RDEVAL        DD4440
131900980204     C                   DELETE    EDRDE000
132000980220      * controllo se c'è proseguimento
132100980204     C                   Z-ADD     2             KNRP1
132200980204     C     KRDE          CHAIN     EDRDE01L                           31
132300980220      *
132400980220     C     *IN31         IFEQ      '0'                                          -- 04 --
132500980204     C                   MOVE      'AAI'         DD4451
132600980204     C                   MOVE      RDEVAL        DD4440
132700980204     C                   DELETE    EDRDE000
132800980220     C                   END                                                    -- 04 --
132900980220      *
133000980220      *  Scrivo record dati note
133100980204     C                   MOVEL     EDSD15        WDAT
133200980204     C                   Z-ADD     §MSNUM        STAPRG
133300980204     C                   MOVEL     'SD15'        STATPR
133400980204     C                   MOVEL     WDAT          STADAT
133500980204     C                   WRITE     IFTSTA00
133600080422     c                   eval      Righe_Dett = 'S'
133700980220     C                   END                                                    -- 03 --
133800980220     C                   END                                                    -- 02 --
133900980220      *-------------------------------
134000980220     C                   ELSE                                                   -- 01 --
134100980220      *-------------------------------
134200980220      *
134300980220      *  Se  è una giacenza cerco le note giacenza
134400961204     C                   Z-ADD     GCPAGC        KAGC
134500961204     C                   Z-ADD     GCPFGC        KFGC
134600961204     C                   Z-ADD     GCPNGC        KNGC
134700961204     C                   Z-ADD     10            KFAS
134800050309     C     KGNP          CHAIN     tignp01L                           31
134900980220     C     *IN31         IFEQ      '0'                                          -- 02 --
135000961204     C                   MOVEL     'AAI'         DD4451
135100961204     C                   MOVEL     GNPDMC        DD4440
135200961204     C*  Scivo record dati note giacenza
135300961204     C                   MOVEL     EDSD15        WDAT
135400961204     C                   Z-ADD     §MSNUM        STAPRG
135500961204     C                   MOVEL     'SD15'        STATPR
135600961204     C                   MOVEL     WDAT          STADAT
135700961204     C                   WRITE     IFTSTA00
135800080422     c                   eval      Righe_Dett = 'S'
135900980220     C                   END                                                    -- 02 --
136000980220     C                   END                                                    -- 01 --
136100961204     C*
136200960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
136300980220     C     WSD15         IFEQ      'S'                                          -- 01 --
136400960821     C                   MOVEL     *BLANKS       WDAT
136500960821     C                   CLEAR                   EDSD15
136600970318     C***                  MOVEL'AAI'     DD4451
136700970318     C***                  MOVELFTX,1     DD4440
136800970318     C***                  MOVELEDSD15    WDAT
136900970318     C***                  Z-ADD§MSNUM    STAPRG
137000970318     C***                  MOVEL'SD15'    STATPR
137100970318     C***                  MOVELWDAT      STADAT
137200970318     C***                  WRITEIFTSTA00
137300970318     C***                  CLEARWSD15
137400980220     C                   END                                                    -- 01 --
137500980220      *
137600960528     C                   ENDSR
137700960528     C*-------------------------------------------------------------*
137800960528     C*? WRSD20 - Scrittura record partner - dettaglio
137900980219      *----------------------------------------------------------------
138000960528     C     WRSD20        BEGSR
138100980219      *
138200980219      *  Pulisco dati DS
138300960912     C                   MOVEA     *BLANKS       SG1
138400960912     C                   MOVEA     *BLANKS       SG2
138500970318     C                   MOVEA     *BLANKS       SG3
138600970619     C                   MOVEA     *BLANKS       SG4
138700960912     C                   MOVEL     *BLANKS       WDAT
138800960528     C                   CLEAR                   EDSD20
138900960528     C                   MOVEL     '99999'       DE1496
139000960729     C                   MOVE      VACNCL        DE7224
139100980220      *
139200990604      *  Se è stata aperta C.A. elaboro a parte
139300990604     C     WELACA        IFEQ      'S'
139400990604     C                   EXSR      COLCA
139500990604     C                   GOTO      ED20
139600990604     C                   ENDIF
139700990604      *
139800990604      *  Se sto effettuando la consegna totale di una bolla con codice
139900980219      *  consegna anomala = '5' su cui è stata effettuata una consegna
140000980219      *  parziale risfleggo eventuali colli trasmessi e chiusi con IDD
140100980220      *
140200980220     C     BLPCCA        IFEQ      '5'
140300970319     C     WTREVE        OREQ      'S'
140400980220      *
140500970319     C     BLPDCM        IFGT      0
140600970318     C     BLPDCP        ANDGT     0
140700970318     C                   EXSR      LIBIDD
140800970318     C                   END
140900980220      *
141000970319     C                   END
141100980220      *
141200980220      *  Se tabellato devo sempre inviare il dettaglio segnacolli
141300980220      *  Carico tutto
141400970304     C     §PTDET        IFEQ      'S'                                          --- 02 ---
141500960912     C                   EXSR      CARCOL
141600980220     C                   EXSR      WRTCOL
141700960912     C                   END
141800980220      *
141900980223      *  Se tabellato devo inviare dettaglio solo per consegne parziali
142000980223      *  Carico non consegnati
142100970304     C     §PTDET        IFEQ      'N'                                          --- 02 ---
142200970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
142300980223      *
142400980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
142500980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
142600970303     C     BLPCCA        OREQ      '5'
142700970617     C     BLPCCA        OREQ      '3'
142800970619     C     VACCCA        OREQ      '7'
142900970619     C     VACCCA        OREQ      'P'
143000970319     C     WTREVE        OREQ      'S'
143100960912     C                   EXSR      CARCOL
143200980220     C                   EXSR      WRTCOL
143300960912     C                   ELSE
143400980223      *
143500980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
143600970304     C     BLPDCM        IFNE      0
143700970304     C                   EXSR      TUTTI
143800970304     C                   END
143900960912     C                   EXSR      WRT20
144000970304     C                   END                                                    --- 03 ---
144100970304     C                   END                                                    --- 02 ---
144200980223      *
144300980223      *  Se previsto da tabella che non devo mai passare il
144400980223      *  numero dei colli scrivo D20
144500970304     C     §PTDET        IFEQ      'M'                                          --- 02 ---
144600980223      *  Per consegne parziali o IDD loop su dettaglio segnacolli
144700970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
144800980223      *
144900980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
145000980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
145100970304     C     BLPCCA        OREQ      '5'
145200970617     C     BLPCCA        OREQ      '3'
145300970619     C     VACCCA        OREQ      '7'
145400970619     C     VACCCA        OREQ      'P'
145500970319     C     WTREVE        OREQ      'S'
145600970304     C                   EXSR      REDBLT
145700970304     C                   ELSE
145800980223      *
145900980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
146000970304     C     BLPDCM        IFEQ      0
146100970304     C                   EXSR      TUTTI
146200970304     C                   END
146300970304     C                   EXSR      WRT20
146400970304     C                   END                                                    --- 03 ---
146500970304     C                   END                                                    --- 02 ---
146600980223      *
146700990604     C     ED20          ENDSR
146800980220      *----------------------------------------------------------------
146900980220      *? LIBIDD - Controllo se devo sfleggare colli con IDD
147000980220      *----------------------------------------------------------------
147100970318     C     LIBIDD        BEGSR
147200980219      *
147300980219      *  Lettura BLT e caricamento colli
147400970318     C     KBLT          CHAIN     FNBLT01L                           31
147500980219      *  rislego i colli da trasmettete al cliente
147600980220     C     *IN31         DOWEQ     '0'
147700980220      *
147800970318     C     BLTFTR        IFEQ      'C'
147900970318     C     BLTDCM        ANDGT     0
148000970318     C     BLTFL1        ANDEQ     '5'
148100970318     C                   MOVEL     ' '           BLTFTR
148200970318     C                   EXCEPT    AGGTRA
148300970318     C                   END
148400980219      * Lettura successiva
148500970318     C     KBLT          READE     FNBLT01L                               31
148600970318     C                   END
148700980219      *
148800970318     C                   ENDSR
148900980219      *----------------------------------------------------------------
149000980219      *? CARCOL - Routine caricamento colli da dettag.
149100980219      *----------------------------------------------------------------
149200960912     C     CARCOL        BEGSR
149300980219      *
149400050413      *  ---------------------
149500050413      *  Legenda dei contatori:   Y1 = Colli Consegnati
149600050413      *  ----------------------   Y2 = Colli Non Consegnati
149700050413      *                           Y3 = Colli Chiusi con IDD
149800050413      *                           Y4 = Colli Negativi con IDD
149900050413      *
150000960912     C                   Z-ADD     0             Y1                5 0
150100960912     C                   Z-ADD     0             Y2                5 0
150200970318     C                   Z-ADD     0             Y3                5 0
150300970619     C                   Z-ADD     0             Y4                5 0
150400980219      *
150500980220      *  Lettura BLT e caricamento colli nelle schiere
150600960912     C     KBLT          CHAIN     FNBLT01L                           31
150700980223      *
150800980220     C     *IN31         DOWEQ     '0'                                          -- 01 --
150900980219      *
151000980219      *  Considero solo i colli non trasmessi al cliente
151100980220     C     BLTFTR        IFNE      'C'                                          -- 02 --
151200980220      *
151300980220      *-------------------------------------------------
151400980219      *  Controllo se devo caricare colli consegnati
151500960912     C     BLTDCM        IFGT      0
151600970303     C     BLTFL1        ANDEQ     ' '
151700970303     C     §PTDET        ANDEQ     'S'
151800970617     C     BLPCCA        ANDNE     '3'
151900980219      *
152000980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
152100980219     C     §PUBS1        IFEQ      'E'
152200980219     C                   EXSR      CARSGE                                       EUROEXPRESS
152300980219     C                   ELSE
152400980219     C                   EXSR      CARSGN                                       BARTOLINI
152500980219     C                   END
152600980219      *
152700980219     C     WSGN          IFNE      *BLANKS
152800960912     C                   ADD       1             Y1
152900960912     C                   MOVEL     WSGN          SG1(Y1)
153000960912     C                   END
153100980219     C                   END
153200980219      *
153300980220      *-------------------------------------------------
153400980219      *  Controllo se devo caricare colli non consegnati
153500960912     C     BLTDCM        IFEQ      0
153600970304     C     §PTDET        ANDNE     'M'
153700970619     C     VACCCA        ANDNE     '7'
153800970304     C     BLTFL1        OREQ      '5'
153900960913     C     §PTDET        ANDNE     'M'
154000970617     C     BLPDCM        ANDGT     0
154100970617     C     BLTFL1        OREQ      '7'
154200970617     C     §PTDET        ANDNE     'M'
154300970617     C     BLTDCM        ANDGT     0
154400970617     C     VACCCA        ANDNE     'P'
154500970617     C     BLPCCA        OREQ      '3'
154600970617     C     §PTDET        ANDNE     'M'
154700970617     C     BLPDCM        ANDGT     0
154800980219      *
154900980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
155000980219     C     §PUBS1        IFEQ      'E'
155100980219     C                   EXSR      CARSGE                                       EUROEXPRESS
155200980219     C                   ELSE
155300980219     C                   EXSR      CARSGN                                       BARTOLINI
155400980219     C                   END
155500980219      *
155600980219     C     WSGN          IFNE      *BLANKS
155700960912     C                   ADD       1             Y2
155800960912     C                   MOVEL     WSGN          SG2(Y2)
155900960912     C                   END
156000980219     C                   END
156100980220      *
156200980220      *-------------------------------------------------
156300980220      *  Controllo se devo caricare colli chiusi con IDD x evento PID
156400970318     C     BLTFL1        IFEQ      '5'
156500970318     C     §PTDET        ANDNE     'M'
156600970318     C     BLPDCM        ANDEQ     0
156700970617     C     BLTFL1        OREQ      '7'
156800970617     C     §PTDET        ANDNE     'M'
156900970617     C     VACCCA        ANDEQ     'P'
157000980220      *
157100980220      *  Se ha segnacollato il cliente carico in schiera i colli cons.
157200980219     C     §PUBS1        IFEQ      'E'
157300980219     C                   EXSR      CARSGE                                       EUROEXPRESS
157400980219     C                   ELSE
157500980219     C                   EXSR      CARSGN                                       BARTOLINI
157600980219     C                   END
157700980219      *
157800980219     C     WSGN          IFNE      *BLANKS
157900970318     C                   ADD       1             Y3
158000970318     C                   MOVEL     WSGN          SG3(Y3)
158100970318     C                   END
158200980220      *
158300980219     C                   END
158400980220      *
158500980220      *-------------------------------------------------
158600980220      *  Se devo inviare solo colli negativi ed ho IDD li carico in
158700980220      *  SG4 (nel caso non abbia ancora attivato flag FL1)
158800970619     C     §PTDET        IFEQ      'N'
158900970619     C     BLTDCM        ANDNE     0
159000970619     C     BLTFL1        ANDEQ     ' '
159100980220      *
159200970619     C     BLPCCA        IFEQ      '3'
159300970619     C     BLPCCA        OREQ      '5'
159400980220      *
159500970619     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
159600980219     C     §PUBS1        IFEQ      'E'
159700980219     C                   EXSR      CARSGE                                       EUROEXPRESS
159800980219     C                   ELSE
159900980219     C                   EXSR      CARSGN                                       BARTOLINI
160000980219     C                   END
160100980219      *
160200980219     C     WSGN          IFNE      *BLANKS
160300970619     C                   ADD       1             Y4
160400970619     C                   MOVEL     WSGN          SG4(Y4)
160500970619     C                   END
160600980219      *
160700980219     C                   END
160800970620     C                   END
160900980220      *
161000980220     C                   END                                                    -- 02 --
161100980220      *
161200050331      * Se è sempre richiesto il dettaglio Segnacolli sul Partner/Cliente ed
161300050331      * è una spedizione di RESO:
161400050331      * (poichè lo status di RESO viene inviata 2 volte da noi con 2 date differenti:
161500050331      *   la prima volta con la data della bolla di reso generata, la seconda con la
161600050331      *    data di consegna della bolla di reso. Vedi FNVAC00T)
161700050331      * ALLORA SOLO IN QUESTO CASO non si fleggano i segnacolli come trasmessi.
161800050331      *  in maniera da poterli sempre ritrasmettere al Partner/Cliente.
161900050331      * (Caso ARVATO che ragiona esclusivamente sui segnacolli e non sul rif.Sped.)
162000050331     C     §PTDET        ifEQ      'S'
162100050331     C     VACCCA        IFEQ      '2'
162200050331     C     VACCCA        OREQ      '6'
162300050331     c                   goto      salta_UPD
162400050331     c                   end
162500050331     c                   end
162600050331      *
162700980220      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
162800970304     C     BLTDCM        IFNE      0
162900970303     C                   MOVEL     'C'           BLTFTR
163000970303     C                   EXCEPT    AGGTRA
163100970304     C                   END
163200050331      *
163300050331     c     salta_UPD     tag
163400980220      *
163500980220      * Lettura successiva
163600960912     C     KBLT          READE     FNBLT01L                               31
163700980220     C                   END
163800980220      *
163900980220     C                   ENDSR
164000990604      *----------------------------------------------------------------
164100990604      *? COLCA - Routine caricamento colli da C.A.
164200990604      *----------------------------------------------------------------
164300990604     C     COLCA         BEGSR
164400990604      *
164500990604     C                   Z-ADD     *ZEROS        Y4                5 0
164600990604     C                   Z-ADD     *ZEROS        WCOLLI            5 0
164700040715      *
164800040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
164900040715      * SIA COME MAMMA CHE COME FIGLIA
165000040715     c                   clear                   fidn12ds
165100040715     c                   eval      i12aas = kAAS
165200040715     c                   eval      i12lnp = kLNP
165300040715     c                   eval      i12nrs = kNRS
165400040715     c                   eval      i12nsp = kNSP
165500040715     c                   eval      i12fel = 'E'
165600040715     c                   eval      i12fan = 'E'
165700040715     c                   eval      i12fch = 'E'
165800040715     c                   eval      i12fpt = 'E'
165900040715     c                   call      'FIDN12R'
166000040715     c                   parm                    fidn12ds
166100040715     c                   z-add     *zeros        ii                2 0
166200040715      **
166300040715      * se trovata almeno una CA
166400040715     c                   if        o12nca > 0
166500040715     c                   dow       ii <  o12nca
166600040715      **
166700040715     c                   add       1             ii
166800040715     c                   movea     skkey(ii)     dskey
166900040715     C                   z-add     dsaac         kaac
167000040715     C                   z-add     dsfil         kfil
167100040715     C                   z-add     dsnca         knca
167200040715     c     kdct          chain     FNDCT000
167300040715     c                   If        %found(fndct01l)
167400040715     C                   MOVEL     DCTFLO        DDCT01
167500040715     C     §DCTtrpt      ifeq      *BLANKS
167600990604      *
167700040715      ** Leggo testata C.A. per reperire il n. spedizione e per flaggarla come
167800040715      ***  trasmessa
167900040715     C**** KBLP          CHAIN     FNDCT000                           31
168000040715      ****
168100040715     C**** *IN31         DOWEQ     '0'                                          -- 01 --
168200040715     C****               MOVEL     DCTFLO        DDCT01
168300040715      ***Considero solo C.A.: non annullate, non chiuse e non trasmesse
168400040715     C**** DCTATB        IFEQ      *BLANKS                                      -- 02 --
168500040715     C**** DCTDCH        ANDEQ     *ZEROS
168600040715     C**** §DCTtrpt      ANDEQ     *BLANKS
168700990607      *
168800990607      *  Flaggo C.A. come trasmessa
168900020412     C                   MOVEL     'S'           §DCTtrpt
169000990607     C                   MOVEL     DDCT01        DCTFLO
169100990607     C                   EXCEPT    AGGDCT
169200990607      *
169300990604      *  Incremento numero colli
169400990604     C                   ADD       DCTNCN        WCOLLI
169500990604      *
169600990604      *  Controllo se devo caricare i segnacolli
169700990604     C     §PTDET        IFNE      'M'                                          -- 03 --
169800990604      *
169900990604      *  Leggo segnacolli C.A.: non annullati, e non chiusi
170000990604     C     KDCD          CHAIN     FNDCD000                           32
170100990607     C     *IN32         DOWEQ     '0'                                          -- 04 --
170200990604     C     DCDATB        IFEQ      *BLANKS                                      -- 05 --
170300990604     C     DCDDCH        ANDEQ     *ZEROS
170400990604      *
170500990604     C     §PUBS1        IFEQ      'E'
170600990604     C                   EXSR      CARSGE                                       EUROEXPRESS
170700990604     C                   ELSE
170800990604     C                   EXSR      CARSGN                                       BARTOLINI
170900990604     C                   ENDIF
171000990604      *
171100990604     C     WSGN          IFNE      *BLANKS
171200990604     C                   ADD       1             Y4
171300990604     C                   MOVEL     WSGN          SG4(Y4)
171400990604     C                   ENDIF
171500990604      *
171600990604     C                   ENDIF                                                  -- 05 --
171700990607     C     KDCD          READE     FNDCD000                               32
171800990604     C                   ENDDO                                                  -- 04 --
171900040715      *
172000040715     C                   ENDIF                                                  -- 03 --
172100040715      *****
172200040715     C*****              ENDIF                                                  -- 02 --
172300040715      **Lettura successiva
172400040715     C*****KBLP          READE     FNDCT000                               31
172500040715     C*****              ENDDO                                                  -- 01 --
172600040715      *****
172700040715     C                   endif
172800040715     C                   endif
172900040715      *
173000040715     c                   enddo
173100040715     c                   endif
173200040715      *
173300990604      *
173400990607      * SCRIVO FLAT FILE
173500990607     C     §PTDET        IFEQ      'M'                                          -- 01 --
173600990607     C     Y4            OREQ      *ZEROS
173700990607     C                   Z-ADD     WCOLLI        DE7224
173800990607     C                   EXSR      WRT20
173900990607     C                   ELSE
174000990607      *
174100990607     C     Y4            IFNE      *ZEROS                                       -- 02 --
174200050427      *  Nel GID imposta il numero dei Colli Danneggiati
174300050427     C                   Z-ADD     y4            DE7224
174400990607     C                   Z-ADD     *ZEROS        WW
174500990607      *
174600990607     C                   DO        Y4            WE                             -- 03 --
174700990607      *  Scrivo ogni §PUEL2 elementi
174800990607     C     WW            IFEQ      §PUEL2                                       -- 04 --
174900990607     C                   MOVEL     '24'          DE4233
175000990607     C                   EXSR      WRT20
175100990607     C                   MOVEL     *BLANKS       EDSD20
175200990607     C                   Z-ADD     *ZEROS        WW
175300990607     C                   ENDIF                                                  -- 04 --
175400990607     C                   ADD       1             WW
175500990607     C                   MOVEL     SG4(WE)       D20(WW)
175600990607     C                   ENDDO                                                  -- 03 --
175700990607      *
175800990607      *  Se ci sono ancora dei colli scrittura nuovo record
175900990607     C     WW            IFNE      *ZEROS                                       -- 03 --
176000990607     C                   MOVEL     '24'          DE4233
176100990607     C                   EXSR      WRT20
176200990607     C                   MOVEL     *BLANKS       EDSD20
176300990607     C                   ENDIF                                                  -- 03 --
176400990607      *
176500990607     C                   ENDIF                                                  -- 02 --
176600990607     C                   ENDIF                                                  -- 01 --
176700990607      *
176800990604     C                   ENDSR
176900980220      *----------------------------------------------------------------
177000980220      *? WRTCOL - Routine scrittura colli
177100980220      *----------------------------------------------------------------
177200980220     C     WRTCOL        BEGSR
177300980220      *
177400980220      *  Scrivo i colli non consegnati
177500980220     C     Y2            IFGT      0
177600960912     C                   MOVE      Y2            DE7224
177700960912     C                   Z-ADD     0             WW                3 0
177800980220      *
177900980220     C                   DO        Y2            WE                3 0
178000980220      *
178100990607      *  Scrivo ogni §PUEL2 elementi
178200990607     C     WW            IFEQ      §PUEL2
178300990607     C                   MOVEL     '24'          DE4233
178400990607     C                   EXSR      WRT20
178500990607     C                   MOVEL     *BLANKS       EDSD20
178600990607     C                   Z-ADD     0             WW
178700990607     C                   ENDIF
178800990607     C                   ADD       1             WW
178900990607     C                   MOVEL     SG2(WE)       D20(WW)
179000990607     C                   ENDDO
179100990607      *
179200990607      *  Se ci sono ancora dei colli scrittura nuovo record
179300990607     C     WW            IFNE      *ZEROS
179400990607     C                   MOVEL     '24'          DE4233
179500990607     C                   EXSR      WRT20
179600990607     C                   MOVEL     *BLANKS       EDSD20
179700990607     C                   ENDIF
179800990607      *
179900990607     C                   ENDIF
180000980220      *
180100980220      *  Scrivo i colli consegnati
180200980220     C     Y1            IFGT      0                                            -- 01 --
180300980223      *
180400980220      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
180500980220      *  (conse. parziale --> ci sono colli non consegnati)
180600980220      *  (pratica IDD --> ci sono colli persi e colli consegnati)
180700980220     C     Y2            IFGT      0                                            -- 02 --
180800081216     C                   ADD       1             WPROG
180900960912     C                   MOVEL     WPROG         DA1490
181000960912     C                   MOVEL     '9999'        DA1312
181100960912     C                   CLEAR                   WDAT
181200960912     C                   MOVEL     EDSD00        WDAT
181300960912     C                   Z-ADD     §MSNUM        STAPRG
181400960912     C                   MOVEL     'SD00'        STATPR
181500960912     C                   MOVEL     WDAT          STADAT
181600960912     C                   WRITE     IFTSTA00
181700080422     c                   eval      Righe_Dett = 'S'
181800980223      *
181900980220      *  Consegna !!!!!!!
182000960912     C                   Z-ADD     1             X
182100980220     C     'CPA'         LOOKUP    C2A(X)                                 32       ?? CONSE.TOT
182200980220      *
182300980220     C     *IN32         IFEQ      '1'                                          -- 03 --
182400980220     C     §PULAB        IFEQ      'S'                                          -- 04 --
182500961022     C                   MOVEL     CEV(X)        DB9013
182600961022     C                   MOVEL     STS(X)        DB9011
182700980220     C                   ELSE                                                   -- 04 --
182800961022     C                   MOVEL     CDV(X)        DB9013
182900961022     C                   MOVEL     STV(X)        DB9011
183000980220     C                   END                                                    -- 04 --
183100960912     C                   MOVEL     '7  '         DB2005
183200960913     C                   MOVEL     BLPDCP        WDATHM
183300960912     C                   MOVE      VACHMC        WDATHM           12 0
183400960912     C                   MOVEL     WDATHM        DB2380
183500960912     C                   MOVEL     '203'         DB2379
183600980220     C                   END                                                    -- 03 --
183700980223      *
183800980223      *  Scrivo record evento CONSEGNA
183900960912     C                   MOVEL     EDSD05        WDAT
184000960912     C                   Z-ADD     §MSNUM        STAPRG
184100960912     C                   MOVEL     'SD05'        STATPR
184200960912     C                   MOVEL     WDAT          STADAT
184300960912     C                   WRITE     IFTSTA00
184400080422     c                   eval      Righe_Dett = 'S'
184500980223      *
184600980223      *  Pulisco SD20
184700960913     C                   CLEAR                   EDSD20
184800960913     C                   MOVEL     '99999'       DE1496
184900980220     C                   END                                                    -- 02 --
185000980223      *
185100980223      *  Scrivo D20 da SG1
185200960912     C                   MOVE      Y1            DE7224
185300960912     C                   Z-ADD     0             WW                3 0
185400980220     C                   DO        Y1            WE                3 0          -- 02 --
185500980223      *
185600990607      *  Scrivo ogni §PUEL2 elementi
185700990607     C     WW            IFEQ      §PUEL2
185800960913     C                   MOVEL     '24'          DE4233
185900960912     C                   EXSR      WRT20
186000990607     C                   MOVEL     *BLANKS       EDSD20
186100990607     C                   Z-ADD     *ZEROS        WW
186200990607     C                   ENDIF
186300990607     C                   ADD       1             WW
186400990607     C                   MOVEL     SG1(WE)       D20(WW)
186500990607     C                   ENDDO                                                  -- 02 --
186600980223      *
186700990607      *  Se ci sono ancora dei colli scrittura nuovo record
186800990607     C     WW            IFNE      *ZEROS                                       -- 02 --
186900990607     C                   MOVEL     '24'          DE4233
187000990607     C                   EXSR      WRT20
187100960912     C                   MOVEL     *BLANKS       EDSD20
187200990607     C                   ENDIF                                                  -- 02 --
187300980223      *
187400980220     C                   ELSE                                                   -- 01 --
187500980223      *
187600980223      *  Scrivo D20 da SG1
187700970619     C                   Z-ADD     Y1            DE7224
187800970619     C                   Z-ADD     0             WW                3 0
187900980220     C                   DO        Y1            WE                3 0          -- 02 --
188000990607      *
188100990607      *  Scrivo ogni §PUEL2 elementi
188200990607     C     WW            IFEQ      §PUEL2
188300990607     C                   MOVEL     '24'          DE4233
188400990607     C                   EXSR      WRT20
188500990607     C                   MOVEL     *BLANKS       EDSD20
188600990607     C                   Z-ADD     *ZEROS        WW
188700990607     C                   ENDIF
188800990607     C                   ADD       1             WW
188900990607     C                   MOVEL     SG1(WE)       D20(WW)
189000990607     C                   ENDDO                                                  -- 02 --
189100990607      *
189200990607      *  Se ci sono ancora dei colli scrittura nuovo record
189300990607     C     WW            IFNE      *ZEROS                                       -- 02 --
189400990607     C                   MOVEL     '24'          DE4233
189500990607     C                   EXSR      WRT20
189600990607     C                   MOVEL     *BLANKS       EDSD20
189700990607     C                   ENDIF                                                  -- 02 --
189800980223      *
189900980220     C                   END                                                    -- 01 --
190000980223      *
190100980223      *  Controllo se devo scrivere colli chiusi x IDD parziale
190200980220     C     Y3            IFGT      0                                            -- 01 --
190300980223      *
190400980223      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
190500980223      *  (conse.parziale --> ci sono colli non consegnati)
190600980223      *  (pratica IDD --> ci sono colli persi e colli consegnati)
190700980220     C     Y2            IFGT      0                                            -- 02 --
190800081216     C                   ADD       1             WPROG
190900970318     C                   MOVEL     WPROG         DA1490
191000970318     C                   MOVEL     '9999'        DA1312
191100970318     C                   CLEAR                   WDAT
191200970318     C                   MOVEL     EDSD00        WDAT
191300970318     C                   Z-ADD     §MSNUM        STAPRG
191400970318     C                   MOVEL     'SD00'        STATPR
191500970318     C                   MOVEL     WDAT          STADAT
191600970318     C                   WRITE     IFTSTA00
191700080422     c                   eval      Righe_Dett = 'S'
191800980223      *
191900980223      *  Consegna !!!!!!!
192000970318     C                   Z-ADD     1             X
192100970619     C     VACCCA        IFEQ      '7'
192200970617     C     'MAN'         LOOKUP    C2A(X)                                 32
192300990607     C                   ELSE
192400970617     C     'PID'         LOOKUP    C2A(X)                                 32
192500990607     C                   END
192600980220      *
192700990607     C     *IN32         IFEQ      '1'                                          -- 03 --
192800990607     C     §PULAB        IFEQ      'S'
192900970318     C                   MOVEL     CEV(X)        DB9013
193000970318     C                   MOVEL     STS(X)        DB9011
193100990607     C                   ELSE
193200970318     C                   MOVEL     CDV(X)        DB9013
193300970318     C                   MOVEL     STV(X)        DB9011
193400990607     C                   END
193500970318     C                   MOVEL     '7  '         DB2005
193600970318     C                   MOVEL     BLPDCP        WDATHM
193700970318     C                   MOVE      VACHMC        WDATHM           12 0
193800970318     C                   MOVEL     WDATHM        DB2380
193900970318     C                   MOVEL     '203'         DB2379
194000990607     C                   END                                                    -- 03 --
194100980223      *
194200980223      *  Scivo record evento CONSEGNA
194300970318     C                   MOVEL     EDSD05        WDAT
194400970318     C                   Z-ADD     §MSNUM        STAPRG
194500970318     C                   MOVEL     'SD05'        STATPR
194600970318     C                   MOVEL     WDAT          STADAT
194700970318     C                   WRITE     IFTSTA00
194800080422     c                   eval      Righe_Dett = 'S'
194900980223      *
195000980223      *  Pulisco SD20
195100970318     C                   CLEAR                   EDSD20
195200970318     C                   MOVEL     '99999'       DE1496
195300990607     C                   END                                                    -- 02 --
195400980223      *
195500980223      *  Scrivo D20 da SG3
195600970318     C                   MOVE      Y3            DE7224
195700970318     C                   Z-ADD     0             WW                3 0
195800990607     C                   DO        Y3            WE                3 0          -- 02 --
195900990607      *
196000990607      *  Scrivo ogni §PUEL2 elementi
196100990607     C     WW            IFEQ      §PUEL2
196200990607     C                   MOVEL     '24'          DE4233
196300990607     C                   EXSR      WRT20
196400990607     C                   MOVEL     *BLANKS       EDSD20
196500990607     C                   Z-ADD     *ZEROS        WW
196600990607     C                   ENDIF
196700990607     C                   ADD       1             WW
196800990607     C                   MOVEL     SG3(WE)       D20(WW)
196900990607     C                   ENDDO                                                  -- 02 --
197000990607      *
197100990607      *  Se ci sono ancora dei colli scrittura nuovo record
197200990607     C     WW            IFNE      *ZEROS                                       -- 02 --
197300990607     C                   MOVEL     '24'          DE4233
197400990607     C                   EXSR      WRT20
197500990607     C                   MOVEL     *BLANKS       EDSD20
197600990607     C                   ENDIF                                                  -- 02 --
197700980220      *
197800990607     C                   ENDIF                                                  -- 01 --
197900990607      *
198000980220      * Per CCA=5 o CCA=3 e  §PTDET='N' Y2, Y3 = 0 e Y4 > 0 imposto Y4 e SG4
198100990607     C     BLPCCA        IFEQ      '5'                                          -- 01 --
198200970619     C     BLPCCA        OREQ      '3'
198300980220      *
198400990607     C     §PTDET        IFEQ      'N'                                          -- 02 --
198500970619     C     Y2            ANDEQ     0
198600970619     C     Y3            ANDEQ     0
198700970619     C     Y4            ANDNE     0
198800970619     C                   Z-ADD     Y4            DE7224
198900970619     C                   Z-ADD     0             WW                3 0
199000990607     C                   DO        Y4            WE                3 0          -- 03 --
199100990607      *
199200990607      *  Scrivo ogni §PUEL2 elementi
199300990607     C     WW            IFEQ      §PUEL2
199400990607     C                   MOVEL     '24'          DE4233
199500990607     C                   EXSR      WRT20
199600990607     C                   MOVEL     *BLANKS       EDSD20
199700990607     C                   Z-ADD     *ZEROS        WW
199800990607     C                   ENDIF
199900990607     C                   ADD       1             WW
200000990607     C                   MOVEL     SG4(WE)       D20(WW)
200100990607     C                   ENDDO                                                  -- 03 --
200200990607      *
200300990607      *  Se ci sono ancora dei colli scrittura nuovo record
200400990607     C     WW            IFNE      *ZEROS                                       -- 03 --
200500990607     C                   MOVEL     '24'          DE4233
200600990607     C                   EXSR      WRT20
200700990607     C                   MOVEL     *BLANKS       EDSD20
200800990607     C                   ENDIF                                                  -- 03 --
200900990607      *
201000990607     C                   END                                                    -- 02 --
201100990607     C                   END                                                    -- 01 --
201200980223      *
201300960912     C                   ENDSR
201400980223      *----------------------------------------------------------------
201500980223      *? TUTTI - fleggo tutti i record di BLT come trasmessi
201600980223      *----------------------------------------------------------------
201700970304     C     TUTTI         BEGSR
201800980223      *
201900980223      *  Aggiorno come trasmessi i singoli colli
202000970303     C     KBLT          CHAIN     FNBLT01L                           31
202100970303     C     *IN31         DOWEQ     '0'
202200980223      *
202300980223      *  Considero solo i colli non trasmessi al cliente
202400970303     C     BLTFTR        IFNE      'C'
202500980223      *
202600980223      *  Fleggo segnacolli come trasmessi se consegnati
202700970303     C     BLTDCM        IFGT      0
202800980223      *
202900980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
203000970303     C                   MOVEL     'C'           BLTFTR
203100970303     C                   EXCEPT    AGGTRA
203200970303     C                   END
203300970304     C                   END
203400980223      *
203500980223      * Lettura successiva
203600970303     C     KBLT          READE     FNBLT01L                               31
203700970303     C                   END
203800980223      *
203900970304     C                   ENDSR
204000980223      *----------------------------------------------------------------
204100980223      *? REDBLT - fleggo tutti i record di BLT come trasmessi
204200980223      *----------------------------------------------------------------
204300970304     C     REDBLT        BEGSR
204400980223      *
204500970304     C                   Z-ADD     0             WCOLCO            5 0
204600970304     C                   Z-ADD     0             WCOLID            5 0
204700980223      *
204800980223      *  Aggiorno come trasmessi i singoli colli
204900970304     C     KBLT          CHAIN     FNBLT01L                           31
205000970304     C     *IN31         DOWEQ     '0'
205100980223      *
205200980223      *  Considero solo i colli non trasmessi al cliente
205300970304     C     BLTFTR        IFNE      'C'
205400980223      *
205500980223      *  Fleggo segnacolli come trasmessi se consegnati
205600970304     C     BLTDCM        IFGT      0
205700970320     C     BLTFL1        OREQ      '5'
205800970617     C     BLTFL1        OREQ      '7'
205900970617     C     BLPCCA        OREQ      '3'
206000980223      *
206100980223      *  Se chiusi con pratica IDD aggiungo 1 al relativo nr.colli
206200980223      *  altrimenti CONTROLLO SE sono colli consegnati
206300970304     C     BLTFL1        IFEQ      '5'
206400970617     C     BLTFL1        OREQ      '7'
206500970617     C     BLPCCA        OREQ      '3'
206600970304     C                   ADD       1             WCOLID
206700970304     C                   ELSE
206800970304     C                   ADD       1             WCOLCO
206900970320     C                   END
207000980223      *
207100980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
207200970304     C                   MOVEL     'C'           BLTFTR
207300970304     C                   EXCEPT    AGGTRA
207400970304     C                   END
207500970304     C                   END
207600980223      *
207700980223      * Lettura successiva
207800970304     C     KBLT          READE     FNBLT01L                               31
207900970304     C                   END
208000980223      *
208100980223      * Se ci sono dei colli chiusi con pratica IDD scrivo TD20 per loro
208200970304     C     WCOLID        IFNE      0
208300970304     C                   MOVE      WCOLID        DE7224
208400970304     C                   EXSR      WRT20
208500980223      *
208600980223      * Se ci sono anche dei colli consegnati normalmente riscrivo SD00 e SD05
208700970304     C     WCOLCO        IFNE      0
208800081216     C                   ADD       1             WPROG
208900970304     C                   MOVEL     WPROG         DA1490
209000970304     C                   MOVEL     '9999'        DA1312
209100970304     C                   CLEAR                   WDAT
209200970304     C                   MOVEL     EDSD00        WDAT
209300970304     C                   Z-ADD     §MSNUM        STAPRG
209400970304     C                   MOVEL     'SD00'        STATPR
209500970304     C                   MOVEL     WDAT          STADAT
209600970304     C                   WRITE     IFTSTA00
209700080422     c                   eval      Righe_Dett = 'S'
209800980223      *
209900980223      * consegna parziale
210000970304     C                   Z-ADD     1             X
210100970304     C     'CPA'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
210200970304     C     *IN32         IFEQ      '1'
210300970304     C     §PULAB        IFEQ      'S'
210400970304     C                   MOVEL     CEV(X)        DB9013
210500970304     C                   MOVEL     STS(X)        DB9011
210600970304     C                   ELSE
210700970304     C                   MOVEL     CDV(X)        DB9013
210800970304     C                   MOVEL     STV(X)        DB9011
210900970304     C                   END
211000970304     C                   MOVEL     '7  '         DB2005
211100970304     C                   MOVEL     BLPDCP        WDATHM
211200970304     C                   MOVE      VACHMC        WDATHM           12 0
211300970304     C                   MOVEL     WDATHM        DB2380
211400970304     C                   MOVEL     '203'         DB2379
211500970304     C                   END
211600980223      *
211700980223      *  Scivo record evento CONSEGNA
211800970304     C                   MOVEL     EDSD05        WDAT
211900970304     C                   Z-ADD     §MSNUM        STAPRG
212000970304     C                   MOVEL     'SD05'        STATPR
212100970304     C                   MOVEL     WDAT          STADAT
212200970304     C                   WRITE     IFTSTA00
212300080422     c                   eval      Righe_Dett = 'S'
212400980223      *
212500980223      *  Pulisco SD20
212600970304     C                   CLEAR                   EDSD20
212700970304     C                   MOVEL     '99999'       DE1496
212800970304     C                   END
212900970304     C                   END
213000980223      *
213100980223      * Scrivo colli consegnati regolarmente
213200970304     C     WCOLCO        IFNE      0
213300970304     C                   MOVE      WCOLCO        DE7224
213400970304     C                   EXSR      WRT20
213500970304     C                   END
213600980223      *
213700970320     C                   ENDSR
213800980220      *----------------------------------------------------------------
213900980220      *? CHKBLT - Controllo se ci sono colli da trasmettere
214000980220      *----------------------------------------------------------------
214100970320     C     CHKBLT        BEGSR
214200980220      *
214300980220      *  Aggiorno come trasmessi i singoli colli
214400970320     C                   MOVEL     'N'           WTRCOL            1
214500970320     C     KBLT          CHAIN     FNBLT01L                           31
214600980220      *
214700970320     C     *IN31         DOWEQ     '0'
214800970320     C     WTRCOL        ANDEQ     'N'
214900980220      *  Considero solo i colli non trasmessi al cliente
215000970320     C     BLTFTR        IFNE      'C'
215100980220      *  Fleggo segnacolli come trasmessi se consegnati
215200970320     C     BLTDCM        IFGT      0
215300970320     C     BLTFL1        OREQ      '5'
215400970320     C                   MOVEL     'S'           WTRCOL
215500970320     C                   END
215600970320     C                   END
215700980220      * Lettura successiva
215800970320     C     KBLT          READE     FNBLT01L                               31
215900970320     C                   END
216000980220      *
216100970320     C                   ENDSR
216200980223      *----------------------------------------------------------------
216300980223      *? WRT20 - scrivo record d20
216400980223      *----------------------------------------------------------------
216500970304     C     WRT20         BEGSR
216600980223      *  Scrivo record SD20
216700960912     C                   MOVEL     EDSD20        WDAT
216800960912     C                   MOVEL     'SD20'        STATPR
216900960912     C                   Z-ADD     §MSNUM        STAPRG
217000960912     C                   MOVEL     WDAT          STADAT
217100960912     C                   WRITE     IFTSTA00
217200080422     c                   eval      Righe_Dett = 'S'
217300980223      *
217400960912     C                   ENDSR
217500980223      *----------------------------------------------------------------
217600980223      *? CARSGE - caricamento segnacollo EUROEXPRESS
217700980223      *----------------------------------------------------------------
217800980219     C     CARSGE        BEGSR
217900980219      *
218000990607     C     VACCCA        IFEQ      'A'
218100990607     C                   Z-ADD     DCDFLS        KFLS
218200990607     C                   Z-ADD     DCDLNA        KLNA
218300990607     C                   Z-ADD     DCDNRS        KNRS
218400990607     C                   Z-ADD     DCDNSC        KNSC
218500990607     C                   ELSE
218600990607     C                   Z-ADD     BLTFLS        KFLS
218700990607     C                   Z-ADD     BLTLNA        KLNA
218800990607     C                   Z-ADD     BLTNRS        KNRS
218900990607     C                   Z-ADD     BLTNSC        KNSC
219000990607     C                   ENDIF
219100050120      *
219200050120     C                   MOVEL     'E'           KTRC
219300050120      *
219400050120      * Controlla se non è Euroexpress ma un cliente normale come Disk C
219500050120     C     kfls          chain     AZORG01L
219600050120     C                   clear                   OG143
219700050120     c                   if        %Found(AZORG01L)
219800050120     C                   movel     ORGDE3        OG143
219900050120     C     §OGntw        IFne      'EEX'
220000050120     C                   z-add     1             KKUT
220100050120     C                   movel     '1B'          KCOD
220200050120     C                   movel(p)  §ptCTM        KKEY
220300050120     C     KTAB2         chain     tabel00f
220400050120     c                   if        %Found(tabel00f)
220500050120     c                   movel     tbluni        ds1B
220600050120      * se si tratta di un Disk "C" non Euroexpress --> Cliente
220700050120     c                   if        §1BF12 <> 'N'
220800050120     C                   MOVEL     'C'           KTRC
220900050120     c                   end
221000050120     c                   end
221100050120     c                   end
221200050120     c                   end
221300050120      *
221400980219     C                   MOVEL     *BLANKS       WSGN
221500980219      *
221600051017     C     KARS          CHAIN     FIARS000                           33
221700051017     C  N33              MOVEL     ARSNOT        WSGN
221800980219      *
221900980219     C                   ENDSR
222000980223      *----------------------------------------------------------------
222100980223      *? CARSGN - caricamento segnacollo
222200980223      *----------------------------------------------------------------
222300980219     C     CARSGN        BEGSR
222400980219      *
222500980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
222600960912     C                   MOVEA     *BLANKS       C35
222700960912     C                   MOVEL     *BLANKS       WSGN             35
222800980219      *  Compongo lnp se la devo passare
222900960912     C     §PULP2        IFNE      0
223000960912     C                   Z-ADD     §PULP2        WA                3 0
223100961029     C                   MOVEL     BLPFLS        WLNP              3
223200960912     C                   MOVEA     WLNP          C35(WA)
223300960912     C                   END
223400980219      *  Compongo lna se la devo passare
223500960912     C     §PULA2        IFNE      0
223600990607     C                   Z-ADD     §PULA2        WA
223700960912     C                   MOVEL     BLPLNA        WLNA              3
223800960912     C                   MOVEA     WLNA          C35(WA)
223900960912     C                   END
224000980219      *  Compongo nrs se lo devo passare
224100960912     C     §PUNR2        IFNE      0
224200990607     C                   Z-ADD     §PUNR2        WA
224300960912     C                   MOVEL     BLPNRS        WNRS              2
224400960912     C                   MOVEA     WNRS          C35(WA)
224500960912     C                   END
224600980219      *  Compongo nsc se la devo passare
224700960912     C     §PUSG2        IFNE      0
224800990607     C                   Z-ADD     §PUSG2        WA
224900990607     C     VACCCA        IFEQ      'A'
225000990604     C                   MOVEL     DCDNSC        WNSC              7
225100990604     C                   ELSE
225200990604     C                   MOVEL     BLTNSC        WNSC
225300990604     C                   END
225400960912     C                   MOVEA     WNSC          C35(WA)
225500960912     C                   END
225600980219      *  Compongo znc se la devo passare
225700960912     C     §PUZN2        IFNE      0
225800990607     C                   Z-ADD     §PUZN2        WA
225900990607     C                   MOVEL     BLPZNC        WZNC              2
226000960912     C                   MOVEA     WZNC          C35(WA)
226100960912     C                   END
226200980219      *  metto C35 in WSGN
226300960912     C                   MOVEA     C35           WSGN
226400980227      *
226500960730     C                   ENDSR
226600960927     C*----------------------------------------------------------------
226700960927     C*? CARNUM - CARICAMENTO NUMERATORE
226800960927     C*----------------------------------------------------------------
226900960927     C     CARNUM        BEGSR
227000960927     C*
227100960927     C                   Z-ADD     0             WPROG             4 0
227200960927     C                   Z-ADD     0             X                 3 0
227300960927     C                   Z-ADD     1             §MSNUM
227400960927     C                   MOVEL     'MS'          KCOD1
227500960927     C                   MOVEL     *BLANKS       KKEY1
227600960927     C                   MOVEL     '784'         KKEY1
227700960927     C     KTABE         CHAIN     EDTAB01L                           30
227800960927     C     *IN30         IFEQ      '0'
227900960927     C     TABFLG        ANDEQ     *BLANKS
228000020412     C                   MOVEL     TABUNI        EDIDSMS
228100960927     C                   ADD       1             §MSNUM
228200061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
228300061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
228400061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
228500061219     c                   move      §MSNUM        campo_4n          4 0
228600061219     c                   if        campo_4n = 0
228700061219     c                   add       1             §MSNUM
228800061219     c                   end
228900960927     C                   Z-ADD     WOGGI         §MSDAT
229000020412     C                   MOVEL     EDIDSMS       TABUNI
229100960927     C                   UPDATE    EDTAB000
229200960927     C                   END
229300980223      *
229400960927     C                   ENDSR
229500980223      *----------------------------------------------------------------
229600980223      *? *INZSR - OPERAZIONI INIZIALI
229700980223      *----------------------------------------------------------------
229800960528     C     *INZSR        BEGSR
229900980223      *
230000080418      *  *entry vecchia gestione con il FNVAC multimebro
230100080418     C**** *ENTRY        PLIST
230200080418     C*******            PARM                    MBR              10            da FNVAC
230300080418     C*******            PARM                    TPR               1            Europolitan
230400080418     C*******            PARM                    TerPar            3            x Filiali
230500080418      *
230600080418      *?  Deve leggere il nuovo TIVGD x generare EDI e fare il Download...  ?
230700080418     C     *ENTRY        PLIST
230800080418     C                   PARM                    CLIENTE           8            "0xxxyyyy"
230900080418     C                   PARM                    Tipo_File         2            "VC"
231000080418     C                   PARM                    Tipo_Scarico      2            "EW"
231100080418     C                   PARM                    Righe_Dett        1            "N"
231200080418     c                   eval      Righe_Dett = 'N'
231300080418      *
231400980223      * Definisco chiavi di accesso
231500080418     C     KEY_VGD01     KLIST
231600080418     C                   KFLD                    Tipo_File                      "VC"
231700080418     C                   KFLD                    CLIENTE                        "0xxxyyyy"
231800080418     C                   KFLD                    Tipo_Scarico                   "EW"
231900080418      *
232000960726     C     KTABE         KLIST
232100960726     C                   KFLD                    KCOD1
232200960726     C                   KFLD                    KKEY1
232300960528     C     KBLP          KLIST
232400960528     C                   KFLD                    KAAS
232500960528     C                   KFLD                    KLNP
232600960528     C                   KFLD                    KNRS
232700960528     C                   KFLD                    KNSP
232800990604     C     KDCD          KLIST
232900990604     C                   KFLD                    DCTAAC
233000990604     C                   KFLD                    DCTFIL
233100990604     C                   KFLD                    DCTNCA
233200960528     C     KGCP          KLIST
233300960528     C                   KFLD                    KAAS
233400960528     C                   KFLD                    KLNP
233500960528     C                   KFLD                    KNRS
233600960528     C                   KFLD                    KNSP
233700960528     C                   KFLD                    KFRG
233800050922     C     KGCp_1        KLIST
233900050922     C                   KFLD                    KAAS
234000050922     C                   KFLD                    KLNP
234100050922     C                   KFLD                    KNRS
234200050922     C                   KFLD                    KNSP
234300961204     C     KGNP          KLIST
234400961204     C                   KFLD                    KAGC
234500961204     C                   KFLD                    KFGC
234600961204     C                   KFLD                    KNGC
234700961204     C                   KFLD                    KFRG
234800961204     C                   KFLD                    KFAS
234900960528     C     KBLT          KLIST
235000960528     C                   KFLD                    KAAS
235100960528     C                   KFLD                    KLNP
235200960528     C                   KFLD                    KNRS
235300960528     C                   KFLD                    KNSP
235400970319     C     KLBL1         KLIST
235500970319     C                   KFLD                    KAAN
235600970319     C                   KFLD                    KLPN
235700970319     C                   KFLD                    KNRN
235800970319     C                   KFLD                    KNSN
235900970319     C     KLBL2         KLIST
236000970319     C                   KFLD                    KAAP
236100970319     C                   KFLD                    KLPP
236200970319     C                   KFLD                    KNRP
236300970319     C                   KFLD                    KNSP
236400060213     C     Kar4          KLIST
236500960528     C                   KFLD                    KAAS
236600960528     C                   KFLD                    KLNP
236700960528     C                   KFLD                    KNRS
236800960528     C                   KFLD                    KNSP
236900960528     C                   KFLD                    KTRC
237000960628     C     KRDE          KLIST
237100960628     C                   KFLD                    KAAS
237200960628     C                   KFLD                    KLNP
237300960628     C                   KFLD                    KNRS
237400960628     C                   KFLD                    KNSP
237500960628     C                   KFLD                    KNMC
237600970319     C                   KFLD                    KNRP1
237700961029     C     KTAB1         KLIST
237800961029     C                   KFLD                    KKUT
237900961029     C                   KFLD                    KCOD
238000970319     C     KTAB2         KLIST
238100970319     C                   KFLD                    KKUT
238200970319     C                   KFLD                    KCOD
238300970319     C                   KFLD                    KKEY
238400970319     C     KEVB          KLIST
238500970319     C                   KFLD                    KAA2
238600970319     C                   KFLD                    KLNP
238700970319     C                   KFLD                    KNRS
238800970319     C                   KFLD                    KNSP
238900970319     C                   KFLD                    KCEV
239000980204     C     KEVB1         KLIST
239100980204     C                   KFLD                    KAA2
239200980204     C                   KFLD                    KLNP
239300980204     C                   KFLD                    KNRS
239400980204     C                   KFLD                    KNSP
239500980204     C                   KFLD                    KDTV
239600980204     C                   KFLD                    KORV
239700980204     C     KEVB2         KLIST
239800980204     C                   KFLD                    KAA2
239900980204     C                   KFLD                    KLNP
240000980204     C                   KFLD                    KNRS
240100980204     C                   KFLD                    KNSP
240200051017     C     KARS          KLIST
240300980219     C                   KFLD                    KFLS
240400980219     C                   KFLD                    KLNA
240500980219     C                   KFLD                    KNRS
240600980219     C                   KFLD                    KNSC
240700980219     C                   KFLD                    KTRC
240800040607     C     KARF          klist
240900040607     C                   kfld                    KAAS
241000040607     C                   kfld                    KLNP
241100040607     C                   kfld                    KNRS
241200040607     C                   kfld                    KNSP
241300040715
241400040715     C     Kdct          klist
241500040715     C                   KFLD                    kaac
241600040715     C                   KFLD                    kfil
241700040715     C                   KFLD                    knca
241800040715      *
241900980223      * Definizione campi di work
242000980219     C     *LIKE         DEFINE    BLTNSC        KNSC
242100980219     C     *LIKE         DEFINE    BLTLNA        KLNA
242200051017     C     *LIKE         DEFINE    ARSFLS        KFLS
242300980219     C     *LIKE         DEFINE    TBLKUT        KKUT
242400961029     C     *LIKE         DEFINE    TBLCOD        KCOD
242500970319     C     *LIKE         DEFINE    TBLKEY        KKEY
242600961029     C     *LIKE         DEFINE    TABCOD        KCOD1
242700960726     C     *LIKE         DEFINE    TABKEY        KKEY1
242800960528     C     *LIKE         DEFINE    BLPAAS        KAAS
242900960528     C     *LIKE         DEFINE    BLPLNP        KLNP
243000960528     C     *LIKE         DEFINE    BLPNRS        KNRS
243100960528     C     *LIKE         DEFINE    BLPNSP        KNSP
243200060213     C     *LIKE         DEFINE    ar4TRC        KTRC
243300960528     C     *LIKE         DEFINE    GCPFRG        KFRG
243400960628     C     *LIKE         DEFINE    RDENMC        KNMC
243500970319     C     *LIKE         DEFINE    RDENRP        KNRP1
243600961204     C     *LIKE         DEFINE    GNPAGC        KAGC
243700961204     C     *LIKE         DEFINE    GNPFGC        KFGC
243800961204     C     *LIKE         DEFINE    GNPNGC        KNGC
243900961204     C     *LIKE         DEFINE    GNPFAS        KFAS
244000970319     C     *LIKE         DEFINE    LBLAAN        KAAN
244100970319     C     *LIKE         DEFINE    LBLLPN        KLPN
244200970319     C     *LIKE         DEFINE    LBLNRN        KNRN
244300970319     C     *LIKE         DEFINE    LBLNSN        KNSN
244400970319     C     *LIKE         DEFINE    LBLAAP        KAAP
244500970319     C     *LIKE         DEFINE    LBLLPP        KLPP
244600970319     C     *LIKE         DEFINE    LBLNRP        KNRP
244700970319     C     *LIKE         DEFINE    EVBAAS        KAA2
244800970319     C     *LIKE         DEFINE    EVBCEV        KCEV
244900980204     C     *LIKE         DEFINE    EVBDTV        KDTV
245000980204     C     *LIKE         DEFINE    EVBORV        KORV
245100980223      *
245200980223      * Recupero codice AS
245300960528     C                   Z-ADD     1             CODUT             1 0
245400960705     C                   CALL      'X§PARUT'
245500020412     C                   PARM                    UT§DSE0F
245600960528     C                   MOVEL     REC80         CNCR80
245700980223      *
245800980223      * Recupero data e ora x messaggio
245900960528     C                   TIME                    WHHDAT           14 0
246000960708     C                   MOVE      WHHDAT        G02DAT
246100960528     C                   MOVE      WHHDAT        DATA              6 0
246200960528     C                   MOVEL     *BLANK        G02ERR
246300960528     C                   CALL      'XSRDA8'
246400960528     C                   PARM                    WLBDA8
246500960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
246600960708     C                   MOVEL     WHHDAT        WHHMM             4 0
246700960528     C                   MOVE      WHHDAT        WANNO             2 0
246800100304     C                   Z-ADD     woggi         WOGGIymd          6 0           UDATE
246900980629      * ---------------------------------------------
247000980629      *  CARICO TABELLE
247100980629      * ---------------------------------------------
247200980223      * Caricamento Tabella eventi con IDD
247300050909      *    o tabella Giacenze/Lasciati Avviso
247400970319     C                   Z-ADD     0             X
247500050909     C                   Z-ADD     0             Xj                3 0
247600050922     C                   Z-ADD     0             Xa                3 0
247700970319     C                   Z-ADD     1             KKUT
247800970319     C                   MOVEL     '2A'          KCOD
247900970319     C                   MOVEA     *BLANKS       IDD
248000050922     C                   MOVEA     *BLANKS       GiaC
248100050926     C                   MOVEA     *BLANKS       LAvv
248200970319     C     KTAB1         CHAIN     TABEL00F                           30
248300970319     C     *IN30         DOWEQ     '0'
248400970319     C     TBLFLG        IFEQ      *BLANKS
248500970319     C                   MOVEL     TBLUNI        DS2A
248600050909      * con IDD
248700970319     C     §2AIDD        IFNE      *BLANKS
248800970319     C                   ADD       1             X
248900970319     C                   MOVEL     TBLKEY        IDD(X)
249000970319     C                   END
249100050909      *
249200050922      * Cod. Giacenza
249300050926     C                   if        §2AFTC = 'G'
249400050909     C                   add       1             Xj
249500050922     C                   MOVEL     TBLKEY        GiaC(Xj)
249600050909     C                   end
249700050922      * Lasciato Avviso
249800050926     C                   if        §2AFTC = 'A'
249900050922     C                   add       1             Xa
250000050926     C                   MOVEL     TBLKEY        LAvv(Xa)
250100050922     C                   end
250200050922      *
250300970319     C                   END
250400970319     C     KTAB1         READE     TABEL00F                               30
250500970319     C                   END
250600980629      *
250700020418      * Caricamento £1
250800030926     c***********        clear                   trul06ds
250900030926     c***********        eval      d06cod = '£1'
251000030924     c*******            movel     simfel        d06key
251100030926     c***********        movel     TerPar        d06key
251200030926     c***********        movel     trul06ds      kpjbu
251300030926     c***********        call      'TRUL06R'
251400030926     c***********        parm                    kpjba
251500030926     c***********        movel     kpjbu         trul06ds
251600030926     c***********        movel     lin           £1
251700030926      *
251800030926     ** carico la schiera dei P.O presenti sull AS
251900030926     c                   clear                   tibs56ds
252000030926     c                   eval      i56ppo = simfel
252100030926     c                   eval      i56mod = 'POS'
252200030926     c                   call      'TIBS56R'
252300030926     c                   parm                    tibs56ds
252400030926     C                   MOVEA     SKI           Pou
252500980629      *
252600980223      * Caricamento Tabella Partner esteri
252700960528     C                   Z-ADD     0             X                 3 0
252800960528     C                   MOVEL     'PT'          TABCOD
252900960528     C     TABCOD        CHAIN     EDTAB01L                           30
253000960528     C     *IN30         DOWEQ     '0'
253100960528     C     TABFLG        IFEQ      *BLANKS
253200960528     C                   ADD       1             X
253300960528     C                   MOVEL     TABKEY        CPT(X)
253400961023     C                   MOVEL     TABKEY        CPU(X)
253500961021     C                   MOVE      '    '        CPT(X)
253600960528     C                   MOVEL     TABUNI        KSC(X)
253700960708     C                   MOVEL     TABUNI        DPT(X)
253800960528     C                   END
253900960528     C     TABCOD        READE     EDTAB01L                               30
254000960528     C                   END
254100980223      *
254200091117      * Deve comunque controllare che i codici caricati dalla PT
254300091117      *  non abbiano un altro codice di riferimento sulla 3K con cui
254400091117      *  impostare il membro.
254500091117     c                   do        x             xx
254600091117     C                   Z-ADD     1             KKUT
254700091117     C                   MOVEL     '3K'          KCOD
254800091117     C                   movel(p)  *zeros        KKEY
254900091117     C                   move      KSC(xx)       KKEY
255000091117     C     KTAB2         chain     tabel00f
255100091117     c                   if        %Found(tabel00f)
255200091117      *
255300091117     c                   movel     tbluni        ds3K
255400091117     c                   if        §3Kcks <> KSC(xx)
255500091117      *  sostituisce il codice con quello del membro
255600091117     c                   move      §3Kcks        Ksc(xx)
255700091117      *
255800091117     c                   end
255900091117     c                   end
256000091117     C                   END
256100980629      *
256200980629      *  In base al codice del PARTNER reperisco identificativo
256300080418     C******             MOVEL     MBR           WMBR              8
256400080418     C******             MOVE      WMBR          WCCM              7 0
256500980629      *
256600080418     C                   MOVE      Cliente       WCCM              7 0
256700020221     C                   Z-ADD     1             XX                3 0
256800980629     C     WCCM          LOOKUP    KSC(XX)                                32
256900020916      *
257000070503      *?  Se non trova la "PT" non deve proseguire   ?
257100070503     C     *IN32         IFEQ      '0'
257200070503     c                   move      'S'           wFINE
257300070503     c                   leaveSR
257400070503     c                   end
257500070503      *
257600980629     C     *IN32         IFEQ      '1'
257700980629      *
257800020412     C                   MOVEL     DPT(XX)       EDIDSPT
257900980629      *
258000980629     C                   MOVEL     'PU'          KCOD1
258100980707     C                   MOVEL     CPU(XX)       KKEY1
258200980707     C     KTABE         CHAIN     EDTAB01L                           30
258300980629      *
258400980629     C     *IN30         IFEQ      '0'
258500980629     C     TABFLG        ANDEQ     *BLANKS
258600020412     C                   MOVEL     TABUNI        EDIDSPU
258700980629     C                   END
258800980629      *
258900980629      * Caricamento Tabella codici eventi. Se richesta Lista Labarta e codici
259000980629      *   Labarta sono Blanks non carico rcd, lo stesso per Lista VGL.
259100980629      *
259200980629     C                   Z-ADD     0             X                 3 0
259300980629     C                   MOVEL     '2A'          TABCOD
259400980629      *
259500980629     C     TABCOD        CHAIN     EDTAB01L                           30
259600980629     C     *IN30         DOWEQ     '0'
259700020412     C                   MOVEL     TABUNI        EDIDS2A
259800980629      *
259900980629     C                   SELECT
260000980629      *
260100980629     C     TABFLG        WHENNE    *BLANKS
260200980629      *
260300980629     C     §PULAB        WHENEQ    'S'
260400980629     C     §2ASTS        ANDEQ     *BLANKS
260500980629     C     §2ACEV        ANDEQ     *BLANKS
260600980629      *
260700980629     C     §PULAB        WHENEQ    *BLANKS
260800980629     C     §2ASTV        ANDEQ     *BLANKS
260900980629     C     §2ACDV        ANDEQ     *BLANKS
261000980629      *
261100980629     C                   OTHER
261200980629     C                   ADD       1             X
261300980629     C                   MOVEL     TABKEY        C2A(X)
261400980629     C                   MOVEL     §2ACEV        CEV(X)
261500980629     C                   MOVEL     §2ASTS        STS(X)
261600980629     C                   MOVEL     §2ACDV        CDV(X)
261700980629     C                   MOVEL     §2ASTV        STV(X)
261800980629     C                   ENDSL
261900980629      *
262000980629     C     TABCOD        READE     EDTAB01L                               30
262100980629     C                   END
262200980629      *
262300980629     C                   END
262400980629      *---------------------------------------
262500980629      *
262600980223      * Caricamento NUMERATORE
262700960927     C                   EXSR      CARNUM
262800980223      * finta read x IFTSTA
262900960528     C   01
263000960528     CANN01              READ      EDIFTSTA                               01
263100980223      *
263200980223      * Controllo se esiste EDRDE
263300980126     C                   Z-ADD     45            LUNG             15 5
263400980126     C                   MOVEL     *BLANKS       COMMAN
263500980126     C                   MOVEA     CMD(1)        COMMAN           80
263600980126     C                   CALL      'QCMDEXC'                            21
263700980126     C                   PARM                    COMMAN
263800980126     C                   PARM                    LUNG
263900980126     C     *IN21         IFEQ      '0'
264000980126     C                   OPEN      EDRDE01L
264100980126     C                   MOVEL     'S'           WTRRDE            1
264200980126     C                   END
264300980223      *
264400960528     C                   ENDSR
264500980223      *-----------------------------------------------------*
264600980223      *   EXCPT   x aggiornamento flag trasmissione BLT     *
264700980223      *-----------------------------------------------------*
264800970303     OFNBLT000  E            AGGTRA
264900970303     O                       BLTFTR
265000990607      *-----------------------------------------------------*
265100990607      *   EXCPT   x aggiornamento flag trasmissione DCT     *
265200990607      *-----------------------------------------------------*
265300990607     OFNDCT000  E            AGGDCT
265400990607     O                       DCTFLO
265500980126**
265600980126CHKOBJ OBJ(EDRDE01L) OBJTYPE(*FILE)
