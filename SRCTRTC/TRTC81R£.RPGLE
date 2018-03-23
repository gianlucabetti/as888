000100930128      ***********************************************************************
000200960528      *   ESPORTA STATO DELLE CONSEGNE VERSO E.D.I.   ESTERO                *
000300930128      ***********************************************************************
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500960528     F*---------------------------------------------------------------------*
000600960528     F*  DATA BASE                                                          *
000700960528     F*---------------------------------------------------------------------*
000800050120     Fazorg01l  IF   E           K DISK
000900050120     F*---------
001000080418     F*****FNVAC00T  IF   E           K DISK
001100080418     FTivgd01L  uF   E           K DISK
001200100304     F                                     INFDS(vgdDS)
001300100304     FTivgd00f  uF   E             DISK    RENAME(TIVGD000:TIVGDfis)
001400960528     F*---------
001500050309     Ftigcp01L  IF   E           K DISK
001600961204     F*---------
001700050309     Ftignp01L  IF   E           K DISK
001800960528     F*---------
001900970319     FFNLBL01L  IF   E           K DISK
002000970319     F*---------
002100970319     FFNLBL02L  IF   E           K DISK
002200970319     F                                     RENAME(FNLBL000:FNLBL002)
002300970319     F*---------
002400060213     Ffiar401L  IF   E           K DISK
002500960528     F*---------
002600960528     FFNBLP01L  IF   E           K DISK
002700960528     F*---------
002800970304     FFNBLT01L  UF   E           K DISK
002900970319     F*---------
003000051017     FFIARS01L  IF   E           K DISK
003100980219      *---------
003200990810     FFNEVB04L  IF   E           K DISK
003300980204     F*---------
003400990810     FFNEVB05L  IF   E           K DISK
003500990810     F                                     RENAME(FNEVB000:FNEVB005)
003600960528     F*---------
003700080418     FEDIFTSTA  iF A E             DISK
003800960628     F*---------
003900980204     FEDRDE01L  UF   E           K DISK    USROPN
004000960528     F*---------
004100960726     FEDTAB01L  UF   E           K DISK
004200961029     F*---------
004300961029     FTABEL00F  IF   E           K DISK
004400990604     F*---------
004500040715     FFNDCT01L  UF   E           K DISK
004600040715     F****FNDCT02L  UF   E           K DISK
004700990604     FFNDCD01L  IF   E           K DISK
004800040607      *--
004900040607      *   x status 20/210 -> Supermercato
005000040607     Ffiarbf1c  IF   E           K DISK
005100040607      *--
005200100304      *
005300100304      * numero relativo di record
005400100304     D  vgdDS          DS
005500100304     D  NrRECvgd             397    400B 0
005600100304      *
005700960528     D*---------------------------------------------------------------------*
005800960528     D* Schiere
005900960528     D*---------------------------------------------------------------------*
006000960528     D* Schiera per scrittura dati località record ST10
006100960528     D* Schiera per scrittura testi liberi record ST15
006200960528     D* Schiera per scrittura testi liberi record SD15
006300960528     D* Schiera per reperimento nr.seganacollo
006400970625     D SG1             S             35    DIM(5000)                            Sgn.conse.
006500970625     D SG2             S             35    DIM(5000)                            Sgn.no con.
006600970625     D SG3             S             35    DIM(5000)                            Sgn.eve.pid
006700970625     D SG4             S             35    DIM(5000)                            IDD NO FL1
006800960528     D* Tabella codici eventi
006900961022     D CEV             S              3    DIM(200)
007000961022     D STS             S              3    DIM(200)
007100961022     D CDV             S              3    DIM(200)
007200961022     D STV             S              3    DIM(200)
007300960528     D C2A             S              3    DIM(200)
007400970319     D* Codici eventi IDD
007500970319     D IDD             S              3    DIM(200)
007600050909     D* Codici Giacenze/Lasciati Avvisi
007700050922     D GiaC            S              3    DIM(200)
007800050926     D LAvv            S              3    DIM(200)
007900960528     D* Tabella partner
008000070503     D CPT             S             35    DIM(300)
008100070503     D CPU             S             35    DIM(300)
008200070503     D DPT             S             90    DIM(300)
008300070503     D KSC             S              7  0 DIM(300)
008400960821     D* TESTO LIBERO
008500970318     D***                 FTX     1   1 70
008600960912     D* composizione schiera 35 chr x segnacollo
008700960912     D C35             S              1    DIM(35)
008800020418     D* Schiera x cariamento filiali gestite su AS
008900030926     D****** £1              S              3  0 DIM(30)                              Fil. gestite
009000030926     D  Pou            s              3  0 INZ
009100030926     D                                     DIM(250)
009200030926
009300980126     D* Esecuzione QCMD x controllare se esiste un CHKOBJ
009400990607     D CMD             S             45    DIM(1) CTDATA PERRCD(1)              QCAEXEC
009500960528     D*---------------------------------------------------------------------*
009600960528     D* DS
009700960528     D*---------------------------------------------------------------------*
009800960528     D* .. per scompozione dati da spedire a seconda del tipo record
009900960528     D EDST00        E DS                  EXTNAME(EDST00DS)
010000960708     D EDST05        E DS                  EXTNAME(EDST05DS)
010100960528     D EDST10        E DS                  EXTNAME(EDST10DS)
010200960528     D  T10                   48    103
010300960528     D                                     DIM(2)
010400960528     D EDST15        E DS                  EXTNAME(EDST15DS)
010500960528     D  T15                   13    362
010600960528     D                                     DIM(5)
010700960528     D EDSD00        E DS                  EXTNAME(EDSD00DS)
010800960528     D EDSD05        E DS                  EXTNAME(EDSD05DS)
010900960528     D EDSD10        E DS                  EXTNAME(EDSD10DS)
011000960528     D EDSD15        E DS                  EXTNAME(EDSD15DS)
011100960528     D  D15                   13    362
011200960528     D                                     DIM(5)
011300960528     D EDSD20        E DS                  EXTNAME(EDSD20DS)
011400960730     D  D20                   33    732
011500960730     D                                     DIM(20)
011600960528     D*  Ds per scrittura dati località
011700960528     D WST10           DS
011800960528     D  WTPL                   1      3
011900960528     D  WLOC                   4     28
012000960528     D*---------------------------------------------------------------------*
012100960528     D* .. per reperimento tipo record di EDIFTSTA da scrivere
012200960528     D*                    .. 5 - 8  : tipo record
012300040218     D WDAT            DS          1950
012400960729     D  STAPRG                 1      4  0
012500960726     D  STATPR                 5      8
012600960528     D*---------------------------------------------------------------------*
012700080418     D fnVACds       E DS
012800960528     D KPJBA         E DS
012900020412     D UT§DSE0F      E DS
013000960528     D CNCR80        E DS
013100050120     D OG143         E DS
013200050120     D ds1B          E DS
013300091117     D ds3K          E DS
013400020412     D EDIDS2A       E DS
013500970319     D DS2A          E DS
013600020412     D EDIDSPT       E DS
013700020412     D EDIDSPU       E DS
013800020412     D EDIDSMS       E DS
013900020412     D DDCT01        E DS
014000960528     D*  DS x scomposizione segnacollo
014100960528     D DSSGN           DS
014200961029     D  SGNFLS                 1      3  0
014300960528     D  SGNLNA                 4      6  0
014400960528     D  SGNNRS                 7      8  0
014500960528     D  SGNNSC                 9     15  0
014600960829     D  SGNZNC                16     17  0
014700960528     D WLBDA8          DS
014800960528     D  G02DAT                 1      8  0
014900960528     D  G02INV                 9     16  0
015000960528     D  G02ERR                17     17
015100960528     D  G02TGI                18     22  0
015200990604      *
015300991019      * DS numero spedizione (da impostare x Europolitan)
015400980122     D DATSPE          DS
015500040305     D  CNI_SPE                3     16  0
015600080418     D   cni_VACAAS            1      4  0
015700080418     D   cni_VACLNP            5      7  0
015800080418     D   cni_VACNRS            8      9  0
015900080418     D   cni_VACNSP           10     16  0
016000080418     D   cni_VACLNA           17     19  0
016100980122     D  FILLER                20     20
016200040715
016300040715     d Fidn12ds      e ds
016400040715     d  skKey                 26    305    dim(20)
016500040715     d  skAnn                306    325    dim(20)
016600040715     d  skDca                326    485    dim(20)
016700040715     d  skFca                486    545  0 dim(20)
016800040715     d  skDch                546    705  0 dim(20)
016900040715     d  skCch                706    745    dim(20)
017000040715
017100040715     d dsKey           ds
017200040715     d  dsaac                         4  0
017300040715     d  dsfil                         3  0
017400040715     d  dsnca                         7  0
017500040715
017600040715     D Kaac            S                   LIKE(dctaac)
017700040715     D Kfil            S                   LIKE(dctfil)
017800040715     D Knca            S                   LIKE(dctnca)
017900020418
018000030926      ******** ds per trul06r
018100030926     d****** trul06ds      e ds
018200030926     d******  lin                    1     90  0 dim(30)
018300030926     D Tibs56ds      E DS
018400030926     D                                     INZ
018500030926     D  Ski                           3  0
018600030926     D                                     DIM(250)
018700030926     D                                     OVERLAY(O56Ski)
018800030926
018900040607     d variata         s              1    INZ('N')
019000081112     d Bolla_variata   s              1    INZ('N')
019100020418
019200960708     D*
019300961122     D COST1           C                   CONST('IT/BAR/IT1   ')
019400060503     D CALB_Plat       C                   CONST('039IT04507990150')
019500960528     I*---------------------------------------------------------------------*
019600960528     I* FILE
019700960528     I*---------------------------------------------------------------------*
019800960528     IIFTSTA00
019900960528     I              SSIFTSTA                    STADAT
020000040607
020100040607     I* GLI INDICATORI DI TIPO RECORD x file Combinato
020200040607     IFNARBD00      11
020300040607     IFNARBK00      12
020400040607     IFIARBT00      13
020500040607     IFNARBG00      14
020600040607     IFNARBM00      15
020700040607     IFNARBV00      16
020800040607     IFNARBP00      17
020900040607      *  INDICI PER:
021000040607      *  - ANNO
021100040607      *  - LINEA PARTENZA
021200040607      *  - SERIE
021300040607      *  - NUMERO SPEDIZIONE
021400040607      *  - DATA VARIAZIONE
021500040607      *  - ORA VARIAZIONE
021600000201      *---------------------------------------------------------------------*
021700000201      * Ciclo principale
021800000201      *---------------------------------------------------------------------*
021900070503      *   Se non è stata trovata la PT nella INZSR deve chiudere il PGM  ?
022000070503     C                   if        WFINE = 'S'
022100070503     c                   goto      fine
022200070503     c                   end
022300080418      *
022400080418      * Posizionamento sul TIVGD x il nuovo ciclo di lettura
022500080418     C                   EXSR      POSFIL
022600000201      *
022700960528      * Posizionamento iniziale sul file  (prima lettura)
022800960528     C                   EXSR      REDFIL
022900000201      *
023000000201      * TESTATA Messaggio
023100000201     C                   if        WFINE <> 'S'
023200000201     C                   EXSR      TESTAMSG
023300000201     C                   endif
023400980220      *
023500980220      * Loop letture FNVAC e scrittura record con dati singola bolla
023600000201     C     WFINE         DOWNE     'S'
023700000201      *
023800000201      * Se ho scritto 999 spedizioni devo scrivere un nuovo msg
023900000201     C                   if        WPROG = 999
024000980223      * Caricamento NUMERATORE
024100960927     C                   EXSR      CARNUM
024200000201      * TESTATA Messaggio
024300000201     C                   EXSR      TESTAMSG
024400000201     C                   endif
024500000201      *
024600000201      *  DETTAGLIO Messaggio
024700000201      *
024800980220      * Eseguo preparazione dati per scrittura record SD00
024900960528     C                   EXSR      WRSD00
025000980220      * Eseguo scrittura dati record SD05 e SD00
025100980220     C                   MOVEL     'N'           WRTEVE            1
025200960528     C                   EXSR      WRSD05
025300980220      *
025400980220      * Scrivo tipi record successivi solo se ho scritto SD00 e SD05
025500980220     C     WRTEVE        IFEQ      'S'
025600980220      * Eseguo scrittura dati record SD10
025700960528     C                   EXSR      WRSD10
025800960528      * Eseguo scrittura dati record SD15
025900960528     C                   EXSR      WRSD15
026000960528      * Eseguo scrittura dati record SD20
026100960528     C                   EXSR      WRSD20
026200980220     C                   END
026300100304      *
026400100304      * Se arrivato qui tutto OK
026500100304      * Aggancia il record fisico per certificare che è andato tutto bene
026600100304      *  e che ha elaborato correttamente il record trasformandolo in EDI.
026700100304     c                   clear                   elab10           10 0
026800100304     c     NrRECvgd      chain     tiVGD00F
026900100304     c                   if        %Found(tiVGD00F)
027000100304     c                   eval      Elab10 = WoggiYMD * 10000 + wHHMM
027100100304     c                   movel     Elab10        vgdPRG
027200100304     c                   update    tiVGDfis
027300100304     c                   end
027400100304      *
027500980220      * Lettura successiva
027600961007     C     NEWREC        TAG
027700960528     C                   EXSR      REDFIL
027800000201     C                   ENDDO
027900980220      * Fine pgm.
028000070503     c     fine          tag
028100000201     C                   eval      *inlr = *on
028200080418      *----------------------------------------------------------------
028300080418      *? POSFIL - Si Posiziona x la LETTURA PRIMO RECORD
028400080418      *----------------------------------------------------------------
028500080418     C     POSFIL        BEGSR
028600080418      *
028700080418      *? Si posiziona con il tipo VC e cliente + l'invio EDI...  ?
028800080418     C     KEY_VGD01     setll     tiVGD01L
028900080418      *
029000080418     C                   ENDSR
029100980220      *----------------------------------------------------------------
029200980220      *? REDFIL - LETTURA PROSSIMO RECORD
029300980220      *----------------------------------------------------------------
029400960528     C     REDFIL        BEGSR
029500980220      *
029600990326     C     LEGGI         TAG
029700990326      *
029800080418      * Lettura file VAC
029900080418     C***********        READ      FNVAC00T                               30
030000080418     C     KEY_VGD01     reade     tiVGD01L                               30
030100990326      *
030200000201     C                   If        *IN30 = *on
030300000201     C                   MOVEL     'S'           WFINE             1
030400000201     C                   Else
030500090219      *
030600090219      *? DEVE LEGGERE solo i record con TIPO invio "EW" da mandare via EDI
030700090219     c                   if        vgdTSC <> 'EW'
030800090219     c                   goto      LEGGI
030900090219     c                   end
031000990326      *
031100080418      *? Una volta letto il TIVGD imposta la relativa DS del VAC...  ?
031200080418     c                   eval      fnVACds = vgdDTA
031300080418      *
031400080418      *? poi aggiorna come già letto il record ...  ?
031500080418     c                   eval      VGDSTO ='S'
031600100304     c                   eval      VGDCNT = VGDCNT + 1
031700080418      *
031800080418      *?                ====================  ?
031900080418     c                   update    tivgd000
032000080418      *?                ====================  ?
032100080418      *
032200051116      *  Pulizia campi riferimento di giacenze della precedente lettura
032300051116     c                   clear                   GCPAGC
032400051116     c                   clear                   GCPFGC
032500051116     c                   clear                   GCPNGC
032600051116      *
032700990326      * Non elaboro codice anomalia IDD3 trasmesso dall'arrivo
032800000201     C                   if        VACCCA = '5'
032900990326     C                   GOTO      LEGGI
033000000201     C                   endif
033100990922      *
033200990922      * Non elaboro SE NON TROVO RCD DI FNBLP
033300990922     C                   Z-ADD     VACAAS        KAAS
033400990922     C                   Z-ADD     VACLNP        KLNP
033500990922     C                   Z-ADD     VACNRS        KNRS
033600990922     C                   Z-ADD     VACNSP        KNSP
033700990922     C     KBLP          CHAIN     FNBLP01L                           31
033800000201     C   31              GOTO      LEGGI
033900000201      *
034000000201     C                   Endif
034100980220      *
034200960528     C                   ENDSR
034300000201      *----------------------------------------------------------------
034400000201      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO
034500000201      *----------------------------------------------------------------
034600000201     C     TESTAMSG      BEGSR
034700000201      *
034800000201      * Eseguo scrittura dati record ST00
034900000201     C                   EXSR      WRST00
035000000201      * Eseguo scrittura dati record ST05
035100000201     C                   EXSR      WRST05
035200000201      * Eseguo scrittura dati record ST10
035300000201     C***                EXSR      WRST10
035400000201      * Eseguo scrittura dati record ST15
035500000201     C                   EXSR      WRST15
035600000201      *
035700000201     C                   ENDSR
035800980629      *----------------------------------------------------------------
035900980629      *? WRST00 - Scrittura record partner
036000980629      *----------------------------------------------------------------
036100960528     C     WRST00        BEGSR
036200980629      *
036300980629      *  Pulisco dati DS
036400960528     C                   CLEAR                   EDST00
036500980629      *  Mittente: Bartolini  (ITBAR + AS..)
036600960528     C                   MOVEL     'ITBAR'       WMITT             8
036700960917     C                   MOVE      '005'         WMITT             8
036800960530     C                   MOVEL     WMITT         TA0004
036900960729     C                   MOVEL     'ZZ'          TA0007
037000980220      *
037100980629      *  In base al codice del PARTNER reperisco identificativo
037200980629     C                   MOVEL     CPT(XX)       TA0010
037300980629      *
037400980220      *  Utilizzo qualificatore x destinatario msg.
037500960827     C                   MOVEL     §PTMSQ        TAA007
037600980220      *  Data/Ora invio messagio: adesso
037700971031     C                   MOVEL     WOGGI         TA0017
037800960528     C                   MOVEL     WHHMM         TA0019
037900980220      *  Codice documento - nome documento
038000960528     C                   MOVEL     '784'         TA1001
038100960913     C                   MOVEL     '      '      TA1000
038200960708     C                   MOVEL     WHHDAT        TA1004                         DATA/ORA -> MSG
038300960528     C                   MOVEL     '9  '         TA1225
038400960528     C                   MOVEL     '137'         TA2005
038500980220      *  Data messaggio: oggi
038600960528     C                   MOVEL     WOGGI         TA2380
038700960528     C                   MOVEL     '102'         TA2379
038800100512      *
038900100512      *    Se si vuole la data con l'orario
039000100512     c                   if        §pudate203 ='S'
039100100512     C                   MOVE      Whhmm         TA2380
039200100512     C                   MOVEL(p)  '203'         TA2379
039300100512     c                   end
039400100512      *
039500980220      *  Scivo record IFTSTA
039600960705     C                   CLEAR                   WDAT
039700960705     C                   MOVEL     EDST00        WDAT
039800960726     C                   Z-ADD     §MSNUM        STAPRG
039900960726     C                   MOVEL     'ST00'        STATPR
040000960705     C                   MOVEL     WDAT          STADAT
040100960528     C                   WRITE     IFTSTA00
040200980220      *
040300960528     C                   ENDSR
040400980220      *----------------------------------------------------------------
040500980220      *? WRST05 - Scrittura record partner
040600980220      *----------------------------------------------------------------
040700960708     C     WRST05        BEGSR
040800980220      *
040900960708     C                   CLEAR                   EDST05
041000980220      *  Ship from:
041100960712     C                   MOVEL     *BLANKS       WDAT
041200960708     C                   MOVEL     'SF '         TE3035
041300970221     C                   MOVEL     §PTPLT        TE3036
041400980220      *  Scivo record IFCSUM
041500960708     C                   CLEAR                   WDAT
041600960708     C                   MOVEL     EDST05        WDAT
041700960726     C                   Z-ADD     §MSNUM        STAPRG
041800960712     C                   MOVEL     'ST05'        STATPR
041900960708     C                   MOVEL     WDAT          STADAT
042000960708     C                   WRITE     IFTSTA00
042100980220      *  Ship to:
042200960712     C                   MOVEL     *BLANKS       WDAT
042300960708     C                   CLEAR                   EDST05
042400960708     C                   MOVEL     'ST '         TE3035
042500970221     C                   MOVEL     COST1         TE3036
042600060503      *
042700060503      *  x CALBERSON --> GEODIS
042800060503      *      imposta la Platform che ci hanno chiesto di utilizzare
042900060503      *      x identificare diversamente dai dati di DUNLOP altrimenti
043000060503      *      non riescono a dividerli nel loro sistema.
043100060503      *  da impostare il §PTLOC ??????
043200060503     c                   if        §PTKSC = 493298
043300060503     C                   clear                   TE3036
043400060503     C                   MOVEL     CALB_Plat     TE3036
043500060503     c                   end
043600060503      *
043700980220      *  Scivo record IFCSUM
043800960708     C                   CLEAR                   WDAT
043900960708     C                   MOVEL     EDST05        WDAT
044000960726     C                   Z-ADD     §MSNUM        STAPRG
044100960712     C                   MOVEL     'ST05'        STATPR
044200960708     C                   MOVEL     WDAT          STADAT
044300960708     C                   WRITE     IFTSTA00
044400980220      *
044500960708     C                   ENDSR
044600980220      *----------------------------------------------------------------
044700980220      *? WRST10 - Scrittura record partner
044800980220      *----------------------------------------------------------------
044900960528     C     WRST10        BEGSR
045000980220      *
045100980220      *  Pulisco dati DS
045200960712     C                   MOVEL     *BLANKS       WDAT
045300960528     C                   CLEAR                   EDST10
045400980220      *  Se devo ritornare imposto AFB + nr.messaggio
045500960726     C                   MOVEL     'AFB'         TB1153
045600960726     C                   MOVEL     §MSNUM        TB1154
045700980220      *  Scivo record IFCSUM
045800960726     C                   CLEAR                   WDAT
045900960726     C                   MOVEL     EDST10        WDAT
046000960726     C                   Z-ADD     §MSNUM        STAPRG
046100960726     C                   MOVEL     'ST10'        STATPR
046200960726     C                   MOVEL     WDAT          STADAT
046300960726     C                   WRITE     IFTSTA00
046400980220      *
046500960528     C                   ENDSR
046600980220      *----------------------------------------------------------------
046700980220      *? WRST15 - Scrittura record partner
046800980220      *----------------------------------------------------------------
046900960528     C     WRST15        BEGSR
047000980220      *
047100980220      *  Pulisco dati DS
047200960712     C                   MOVEL     *BLANKS       WDAT
047300960528     C                   CLEAR                   EDST15
047400980220      *
047500960528     C                   ENDSR
047600980220      *----------------------------------------------------------------
047700980220      *? WRSD00 - Scrittura record partner - dettaglio
047800980220      *----------------------------------------------------------------
047900960528     C     WRSD00        BEGSR
048000980220      *
048100980220      *  Pulisco dati DS
048200960712     C                   MOVEL     *BLANKS       WDAT
048300960528     C                   CLEAR                   EDSD00
048400980220      *  Controllo se la bolla figlia con bolla originale su stesso AS
048500980220      *  non invio le date di consegna
048600970319     C                   Z-ADD     VACAAS        KAAN
048700970319     C                   Z-ADD     VACLNP        KLPN
048800970319     C                   Z-ADD     VACNRS        KNRN
048900970319     C                   Z-ADD     VACNSP        KNSN
049000970319     C     KLBL1         CHAIN     FNLBL01L                           31
049100970319     C     *IN31         IFEQ      '0'
049200030926     C***  LBLLPO        LOOKUP    £1                                     31
049300030926     C     LBLLPO        LOOKUP    Pou                                    31
049400970319     C   31              GOTO      NEWREC
049500970319     C                   END
049600980220      *
049700980220      *  Imposto numero spedizione originale cliente/partner
049800980220      *
049900961022     C                   MOVEL     *BLANKS       WNRSPE           35
050000960528     C                   Z-ADD     VACAAS        KAAS
050100960528     C                   Z-ADD     VACLNP        KLNP
050200960528     C                   Z-ADD     VACNRS        KNRS
050300960528     C                   Z-ADD     VACNSP        KNSP
050400040305      * La regola è:
050500040305      *  si deve restituire il Nostro Nr.spedizione nel CNI invece
050600060213      *   dal ar4 --> deve essere impostato l'RFF+AGE ossia Rif.Partner
050700080418      *
050800080418     c                   eval      cni_VACAAS = VACAAS
050900080418     c                   eval      cni_VACLNP = VACLNP
051000080418     c                   eval      cni_VACNRS = VACNRS
051100080418     c                   eval      cni_VACNSP = VACNSP
051200080418     c                   eval      cni_VACLNA = VACLNA
051300100209      *
051400100209     C********           MOVEL     CNI_spe       DA1004
051500040305      *
051600040305      * quindi occorre impostare diversamente i campi della DS00 e DS05
051700040305      *
051800960708     C                   MOVEL     'E'           KTRC
051900060213     C     Kar4          CHAIN     fiar401L                           31
052000980220      *
052100100722      * se non richiesta forzatura del Riferimento numerico da restituire
052200100722     c                   if        *in31 = '0' and §ptSGN =' '
052300100722     C**** *IN31         IFEQ      '0'
052400060213     C*****              MOVEL     ar4NOT        DA1004
052500060213     C                   MOVEL     ar4NOT        Wnrspe
052600960821     C                   ELSE
052700980220      *
052800060213      * Se non trovo ar4NOT utilizzo in cascata Rif.Mittente Alfa e numerico
052900980220      * (????) Segnalo se manca ricezione EDI e sped. bollettata a mano
053000980220      *
053100980220     C                   MOVEL     'S'           WSD15             1
053200960821     C     KBLP          CHAIN     FNBLP01L                           31
053300960821     C     *IN31         IFEQ      '0'
053400100722      * se non richiesta forzatura del Riferimento numerico da restituire
053500960821     C     BLPRMA        IFNE      *BLANKS
053600100722     C     §ptsgn        andeq     *BLANKS
053700040305     C******             MOVEL     BLPRMA        DA1004
053800040305     C                   MOVEL     BLPRMA        Wnrspe
053900960821     C                   ELSE
054000040305     C******             MOVEL     BLPRMN        DA1004
054100040305     C                   MOVEL     BLPRMN        Wnrspe
054200961007     C                   ENDIF
054300100304      **********
054400980220      * Se non ho impostato DA1004 leggo rcd successivo
054500100304     C**** DA1004        IFEQ      *BLANKS
054600100304     C**********         GOTO      NEWREC
054700100304     C**********         ENDIF
054800100304      **********
054900960821     C                   ENDIF
055000960708     C                   END
055100980220      *
055200040305     C*********          MOVEL     DA1004        WNRSPE
055300100209      *
055400100209      * Default
055500100209      *  se non richiesta l'inversione del dato CNI nell'AGE
055600100209     c                   if        §ptTRD <> 'S'
055700100209     C                   MOVEL(p)  CNI_spe       DA1004
055800100209      *  inverte con l'AGE
055900100209     c                   elseIF    §ptTRD  = 'S'
056000100209     C                   MOVEL(p)  Wnrspe        DA1004
056100100209      *
056200100209     c                   end
056300100304      *
056400100304      * Se non ho impostato DA1004 leggo rcd successivo
056500100304     C     DA1004        IFEQ      *BLANKS
056600100304     C                   GOTO      NEWREC
056700100304     C                   ENDIF
056800980220      *
056900960528     C                   ENDSR
057000980220      *----------------------------------------------------------------
057100980220      *? WRSD05 - Scrittura record partner - dettaglio
057200980220      *----------------------------------------------------------------
057300960528     C     WRSD05        BEGSR
057400980220      *
057500980220      *  Pulisco dati DS
057600960712     C                   MOVEL     *BLANKS       WDAT
057700960528     C                   CLEAR                   EDSD05
057800980220      *
057900980220      *  Se ho l'obbligo di ritornaglielo scrivo nr. CMR altrimenti nr. sped.
058000980220     C     §PURFF        IFEQ      'C'
058100980220     C                   EXSR      NUMCMR
058200980220     C                   ELSE
058300961022     C                   MOVEL     'AGE'         DB1153
058400100209      *
058500100209      * Default
058600100209      *  se non richiesta l'inversione del dato CNI nell'AGE
058700100209     c                   if        §ptTRD <> 'S'
058800961022     C                   MOVEL     WNRSPE        DB1154
058900100209      *  inverte con il CNI
059000100209     c                   elseIF    §ptTRD  = 'S'
059100100209     C                   MOVEL     CNI_spe       DB1154
059200100209      *
059300100209     c                   end
059400100209      *
059500980220     C                   END
059600980220      *
059700991019      * Per Europolitan imposto numero spedizione dalle pos.16-35
059800080418     C**** TPR           IFEQ      'S'
059900080418     C**********         MOVE      DATSPE        DB1154
060000080418     C**********         END
060100980220      *------------------------------------------------------*
060200980220      *  Se la bolla è una mamma controllo se è stato effettuato un evento IDD
060300970319     C                   MOVEL     'N'           WTREVE            1
060400970319     C                   Z-ADD     VACAAS        KAAP
060500970319     C                   Z-ADD     VACLNP        KLPP
060600970319     C                   Z-ADD     VACNRS        KNRP
060700970319     C                   Z-ADD     VACNSP        KNSP
060800970319     C     KLBL2         CHAIN     FNLBL02L                           31
060900980220      *
061000980220      *  Scorro schiera eventi IDD e controllo se ce ne sono
061100980220     C     *IN31         IFEQ      '0'
061200970319     C                   Z-ADD     1             X
061300980220      *
061400980220     C     IDD(X)        DOWNE     *BLANKS
061500970319     C     X             ANDLE     200
061600970319     C     WTREVE        ANDEQ     'N'
061700970319     C                   MOVEL     IDD(X)        KCEV
061800970319     C                   Z-ADD     VACAAS        KAA2
061900970319     C                   Z-ADD     VACLNP        KLNP
062000970319     C                   Z-ADD     VACNRS        KNRS
062100970319     C                   Z-ADD     VACNSP        KNSP
062200990810     C     KEVB          CHAIN     FNEVB04L                           31
062300980220     C  N31              MOVEL     'S'           WTREVE
062400980220     C   31              ADD       1             X
062500980220     C                   END
062600980220      *
062700980220     C                   END
062800980220      *------------------------------------------------------*
062900990604      *  APERTURA C.A.
063000990604     C                   MOVEL     *BLANKS       WELACA            1
063100990604     C     VACCCA        IFEQ      'A'                                          -- 01 --
063200990604      *
063300990604      *  Controllo se ho C.A. aperte non trasmesse
063400040715     C**** KBLP          CHAIN     FNDCT000                           31
063500040715      ****
063600040715     C**** *IN31         DOWEQ     *OFF                                         -- 02 --
063700040715     C****               MOVEL     DCTFLO        DDCT01
063800040715     C**** DCTATB        IFEQ      *BLANKS
063900040715     C**** DCTDCH        ANDEQ     *ZEROS
064000040715     C**** §DCTtrpt      ANDEQ     *BLANKS
064100040715     C****               MOVEL     'S'           WELACA
064200040715     C****               SETON                                        31
064300040715     C****               ELSE
064400040715     C**** KBLP          READE     FNDCT000                               31
064500040715     C****               ENDIF
064600040715     C****               ENDDO                                                  -- 02 --
064700040715      *
064800040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
064900040715      * SIA COME MAMMA CHE COME FIGLIA
065000040715     c                   clear                   fidn12ds
065100040715     c                   eval      i12aas = kAAS
065200040715     c                   eval      i12lnp = kLNP
065300040715     c                   eval      i12nrs = kNRS
065400040715     c                   eval      i12nsp = kNSP
065500040715     c                   eval      i12fel = 'E'
065600040715     c                   eval      i12fan = 'E'
065700040715     c                   eval      i12fch = 'E'
065800040715     c                   eval      i12fpt = 'E'
065900040715     c                   call      'FIDN12R'
066000040715     c                   parm                    fidn12ds
066100040715     c                   z-add     *zeros        ii                2 0
066200040715      **
066300040715      * se trovata almeno una CA
066400040715     c                   if        o12nca > 0
066500040715     c                   dow       ii <  o12nca
066600040715      **
066700040715     c                   add       1             ii
066800040715     c                   movea     skkey(ii)     dskey
066900040715     C                   z-add     dsaac         kaac
067000040715     C                   z-add     dsfil         kfil
067100040715     C                   z-add     dsnca         knca
067200040715     c     kdct          chain(n)  FNDCT000
067300040715     c                   If        %found(fndct01l)
067400040715     C                   MOVEL     DCTFLO        DDCT01
067500040715     C     §DCTtrpt      ifeq      *BLANKS
067600040715     C                   MOVEL     'S'           WELACA
067700040715     C                   leave
067800040715     C                   endif
067900040715     C                   endif
068000040715      *
068100040715     c                   enddo
068200040715     c                   endif
068300990604      *
068400990604     C     WELACA        IFEQ      'S'                                          -- 02 --
068500990604      *  Determino la causale evento e controllo se reso codificato in tabella
068600991019      *   IDD per mancanze EuroExpress ed Europolitan; RFD per Avarie e Ammanchi
068700991019      *    EuroExpress; per Avarie e Ammanchi Europolitan non trasmetto nulla.
068800990604     C                   Z-ADD     1             X
068900991019     C                   SETOFF                                       32
069000990604      *
069100991019     C                   SELECT
069200080418     C**** TPR           WHENEQ    'S'                                          Europolitan
069300080418     C**** DCTTAD        ANDNE     '01'                                            no
069400080418     C**** DCTTAD        ANDNE     '21'                                          mancanze
069500991019      *
069600080418     C**** TPR           WHENEQ    'S'                                          Europolitan
069700080418     C**** 'IDD'         LOOKUP    C2A(X)                                 32
069800991019      *
069900991019     C     DCTTAD        WHENNE    '01'                                         EuroExpress
070000991019     C     DCTTAD        ANDNE     '21'                                         mancanze
070100990604     C     'RFD'         LOOKUP    C2A(X)                                 32
070200991019      *
070300991019     C                   OTHER                                                  EuroExpress altro
070400990604     C     'IDD'         LOOKUP    C2A(X)                                 32
070500991019     C                   ENDSL
070600990604      *
070700990604     C     *IN32         IFEQ      '1'                                          -- 03 --
070800990604     C     §PULAB        IFEQ      'S'
070900990604     C                   MOVEL     CEV(X)        DB9013
071000990604     C                   MOVEL     STS(X)        DB9011
071100990604     C                   ELSE
071200990604     C                   MOVEL     CDV(X)        DB9013
071300990604     C                   MOVEL     STV(X)        DB9011
071400990604     C                   ENDIF
071500990604     C                   MOVEL     '7  '         DB2005
071600990604     C                   MOVEL     VACDCM        WDATHM
071700990604     C                   MOVE      VACHMC        WDATHM           12 0
071800990604     C                   MOVEL     WDATHM        DB2380
071900990604     C                   MOVEL     '203'         DB2379
072000990604      *  Scrivo record SD00 e SD05
072100990604     C                   EXSR      WRT0E5
072200990604     C                   ENDIF                                                  -- 03 --
072300990604      *
072400990604     C                   ENDIF                                                  -- 02 --
072500990608     C                   GOTO      FIND05
072600990604     C                   ENDIF                                                  -- 01 --
072700990604      *------------------------------------------------------*
072800980220      *  Controllo se è stata effettuata un consegna parziale
072900970319     C                   Z-ADD     VACAAS        KAAS
073000970319     C                   Z-ADD     VACLNP        KLNP
073100970319     C                   Z-ADD     VACNRS        KNRS
073200970319     C                   Z-ADD     VACNSP        KNSP
073300970319     C     KBLP          CHAIN     FNBLP01L                           31
073400980220      *
073500980220     C     BLPDCP        IFGT      0
073600960729     C     BLPDCM        ANDEQ     0
073700980220      *
073800980220      *  Controllo se devo trasmettere i colli
073900980220     C     WTREVE        IFEQ      'S'
074000970320     C                   EXSR      CHKBLT
074100980220      *  Se  non ci sono colli da trasmettere passo al record successivo
074200980220     C     WTRCOL        IFEQ      'N'
074300980220     C                   GOTO      NEWREC
074400980220     C                   END
074500980220     C                   END
074600980220      *
074700960528     C                   Z-ADD     1             X
074800970617     C                   SETOFF                                       32
074900980220      *
075000970617     C     §PTDET        IFEQ      'M'
075100970320     C     BLPCCA        ANDEQ     '5'
075200970320     C     §PTDET        OREQ      'M'
075300970320     C     WTREVE        ANDEQ     'S'
075400970320     C     'PID'         LOOKUP    C2A(X)                                 32
075500970320     C                   ELSE
075600970320     C     'P  '         LOOKUP    C2A(X)                                 32
075700970320     C                   END
075800980220      *
075900980220     C   32§PULAB        IFEQ      'S'
076000960528     C                   MOVEL     CEV(X)        DB9013
076100960612     C                   MOVEL     STS(X)        DB9011
076200961022     C                   ELSE
076300961022     C                   MOVEL     CDV(X)        DB9013
076400961022     C                   MOVEL     STV(X)        DB9011
076500980220     C                   END
076600980220      *
076700960528     C                   MOVEL     '7  '         DB2005
076800960528     C                   MOVEL     BLPDCP        DB2380
076900960528     C                   MOVEL     '102'         DB2379
077000100512      *
077100100512      *    Se si vuole la data con l'orario
077200100512     c                   if        §pudate203 ='S'
077300100512     C                   MOVE      '0001'        TA2380
077400100512     C                   MOVEL(p)  '203'         TA2379
077500100512     c                   end
077600100512      *
077700980220      *  Scrivo record SD00 e SD05
077800980220     C                   EXSR      WRT0E5
077900960528     C                   GOTO      FIND05
078000980220     C                   END
078100990604      *-------------------------------------------------------------*
078200050922      * aggancia sempre la giacenza se c'è stata giacenza e non è consegnata
078300050922     C     vacDAG        ifGT      0                                             -- 0A --
078400050922     C     vacDCM        andEQ     0
078500050922     C                   Z-ADD     VACAAS        KAAS
078600050922     C                   Z-ADD     VACLNP        KLNP
078700050922     C                   Z-ADD     VACNRS        KNRS
078800050922     C                   Z-ADD     VACNSP        KNSP
078900050922     c                   Z-ADD     0             KFRG
079000050922     C     KGCP          CHAIN     tigcp01L                           31
079100050922     c                   if        %Found(tigcp01L)                              -- 0B --
079200050922
079300050922      * imposta la data Distinta/udate apertura giacenza fuori distinta
079400050922     c                   z-add     GCPDXD        vacDAG
079500050922
079600980220      *  Se la data più alta è di giacenza invio data apertura giacenza
079700970617     C     VACDAG        IFGT      VACDLA                                       -- 01 --
079800960528     C     VACDAG        ANDGT     VACDCM
079900960528     C     VACDAG        ANDGT     0
080000050922     C****               Z-ADD     0             KFRG
080100050922     C***  KGCP          CHAIN     tigcp01L                           31
080200980220      *  Controllo se il codice apertura giacenza è codificato in tabella
080300960528     C                   Z-ADD     1             X
080400960528     C     GCPCMC        LOOKUP    C2A(X)                                 32
080500980220      *
080600970617     C     *IN32         IFEQ      '1'                                          -- 02 --
080700980220      *
080800980220     C     §PULAB        IFEQ      'S'                                          -- 03 --
080900960528     C                   MOVEL     CEV(X)        DB9013
081000960612     C                   MOVEL     STS(X)        DB9011
081100980220     C                   ELSE                                                   -- 03 --
081200961022     C                   MOVEL     CDV(X)        DB9013
081300961022     C                   MOVEL     STV(X)        DB9011
081400980220     C                   END                                                    -- 03 --
081500980220      *
081600960528     C                   MOVEL     '7  '         DB2005
081700050909      * ----->
081800050919      *   Imposta la Data e le ore 18:00 fisse. Prima prendeva sempre la data
081900050919      *   apertura/riapertura della giacenza invece,
082000050919      *    questa è la data della distinta di consegna da ARB oppure
082100050919      *    la UDATE se si è aperta giacenza non passando da distinta.
082200050919     C                   movel     gcpDXD        WDATHM
082300050909     C                   move      '1800'        WDATHM
082400050909     C                   movel     WDATHM        DB2380
082500050909      *
082600041201      * attenzione se si tratta di una riapertura giacenza deve comunicare
082700041201      *  la data di ultima riapertura assieme al motivo di giacenza.
082800050919     c*********          if        gcpDUR > vacDAG
082900050919      *********
083000050919     C*********          z-add     gcpDUR        data08            8 0
083100050919     C*********          MOVEL     data08        DB2380
083200050919     C*********          movel     data08        WDATHM
083300050919     C*********          move      '1800'        WDATHM
083400050919     C*********          movel     WDATHM        DB2380
083500050919      *********
083600050919     c*********          else
083700050919      *********
083800050919     C*********          MOVEL     VACDAG        DB2380
083900050919     C*********          movel     vacdag        WDATHM
084000050919     C*********          move      '1800'        WDATHM
084100050919     C*********          movel     WDATHM        DB2380
084200050919      *********
084300050919     c*********          end
084400050919      *********
084500050922      *
084600050922      * Controlla se è stato fatto un evento di giacenza senza aver
084700050922      *  successivamente riaperto Giacenza ... prende questo come Data/Ora
084800050922     C     KGCP_1        setgt     fnEVB04L
084900050922     C     KGCP_1        readpe    fnEVB04L
085000050922     c                   dow       not %EoF(fnEVB04L)
085100050922     c     evbCEV        lookup    GiaC                                   31
085200050922     c                   if        *in31 = *on
085300050922     c                   if        evbDEV > gcpDXD
085400050922     C                   movel     evbDEV        WDATHM
085500050922     C                   move      evbHEV        WDATHM
085600050922     C                   movel     WDATHM        DB2380
085700050922     c                   end
085800050922     c                   leave
085900050922     c                   end
086000050922     C     KGCP_1        readpe    fnEVB04L
086100050922     c                   enddo
086200050922      *********
086300050919     C*********          MOVEL     '102'         DB2379
086400050909     C                   movel     '203'         DB2379
086500050922      *
086600980220      *  Scrivo record SD00 e SD05
086700980220     C                   EXSR      WRT0E5
086800980220     C                   GOTO      FIND05
086900970617     C                   END                                                    -- 02 --
087000970617     C                   END                                                    -- 01 --
087100050922
087200050922     c                   end                                                    -- 0B --
087300050922     c                   end                                                    -- 0A --
087400980220      *-------------------------------------------------------------*
087500980220      *  La spedizione ha avuto lasciato avviso
087600980220     C     VACDLA        IFGT      VACDCM
087700960528     C     VACDLA        ANDGT     0
087800980220     C*  Controllo se il codice lasciato avviso è codificato in tabella
087900960528     C                   Z-ADD     1             X
088000960528     C     'AVV'         LOOKUP    C2A(X)                                 32
088100980220      *
088200960528     C     *IN32         IFEQ      '1'
088300980220      *
088400961022     C     §PULAB        IFEQ      'S'
088500960528     C                   MOVEL     CEV(X)        DB9013
088600960612     C                   MOVEL     STS(X)        DB9011
088700961022     C                   ELSE
088800961022     C                   MOVEL     CDV(X)        DB9013
088900961022     C                   MOVEL     STV(X)        DB9011
089000961022     C                   END
089100980220      *
089200960528     C                   MOVEL     '7  '         DB2005
089300050926     C******             MOVEL     VACDLA        DB2380
089400050926     C******             MOVEL     '102'         DB2379
089500050926     C                   movel     VACDLA        WDATHM
089600050926     C                   move      1800          WDATHM
089700050926     C                   movel     WDATHM        DB2380
089800050926     C                   MOVEL     '203'         DB2379
089900050922      *
090000050922      * Controlla se è stato fatto un evento di L.AVV. posteriore
090100050922      *  a quello presente nel vacDLA ... prende questo come Data/Ora
090200050922     C                   Z-ADD     VACAAS        KAAS
090300050922     C                   Z-ADD     VACLNP        KLNP
090400050922     C                   Z-ADD     VACNRS        KNRS
090500050922     C                   Z-ADD     VACNSP        KNSP
090600050922     C     KGCP_1        setgt     fnEVB04L
090700050922     C     KGCP_1        readpe    fnEVB04L
090800050922     c                   dow       not %EoF(fnEVB04L)
090900050926     c     evbCEV        lookup    LAvv                                   31
091000050922     c                   if        *in31 = *on
091100050922     c                   if        evbDEV >= vacDLA
091200050922     C                   movel     evbDEV        WDATHM
091300050922     C                   move      evbHEV        WDATHM
091400050922     C                   movel     WDATHM        DB2380
091500050922     C                   MOVEL     '203'         DB2379
091600050922     c                   end
091700050922     c                   leave
091800050922     c                   end
091900050922     C     KGCP_1        readpe    fnEVB04L
092000050922     c                   enddo
092100050922
092200980220      *  Scrivo record SD00 e SD05
092300980220     C                   EXSR      WRT0E5
092400980220     C                   GOTO      FIND05
092500960528     C                   END
092600970617     C                   END                                                    -- 01 --
092700980220      *-------------------------------------------------------------*
092800081117      *
092900081117      *  -> STATUS 20/210 Appuntamento (SUPERMERCATO)
093000081117      * Non è un evento ma una particolarità di consegna
093100081117      *  aggiunto anche per appuntamento (Booking ins)
093200081117     c                   eval      Bolla_variata  = 'N'
093300081117      *
093400081117     c                   if        VACTC1 = 'S' or VACTC2 = 'S' or
093500081117     c                             VACTC1 = 'A' or VACTC2 = 'A'
093600081117     c                   Exsr      STATUS_20_210
093700081117     c                   end
093800081117      *
093900081117      *-------------------------------------------------------------*
094000980220      *  La spedizione è stata chiusa per: RESO - IDD - CONSEGNA
094100970617     C     VACDCM        IFGT      0                                            -- 01 --
094200970617     C     VACCCA        ANDNE     '7'
094300970617     C     VACCCA        ANDNE     'P'
094400970617     C     BLPDCM        ORGT      0
094500980220      *
094600980220      *---------------------------------*
094700980220      *  RESO
094800970617     C     VACCCA        IFEQ      '2'                                          -- 02 --
094900960712     C     VACCCA        OREQ      '6'
095000980220      *
095100980220      *  Controllo se reso codificato in tab.
095200960528     C                   Z-ADD     1             X
095300980220     C     'RES'         LOOKUP    C2A(X)                                 32      ??? RESO
095400970617     C     *IN32         IFEQ      '1'                                          -- 03 --
095500961022     C     §PULAB        IFEQ      'S'
095600960528     C                   MOVEL     CEV(X)        DB9013
095700960612     C                   MOVEL     STS(X)        DB9011
095800961022     C                   ELSE
095900961022     C                   MOVEL     CDV(X)        DB9013
096000961022     C                   MOVEL     STV(X)        DB9011
096100961022     C                   END
096200960528     C                   MOVEL     '7  '         DB2005
096300960528     C                   MOVEL     VACDCM        WDATHM
096400960528     C                   MOVE      VACHMC        WDATHM           12 0
096500960528     C                   MOVEL     WDATHM        DB2380
096600960528     C                   MOVEL     '203'         DB2379
096700980220      *  Scrivo record SD00 e SD05
096800980220     C                   EXSR      WRT0E5
096900980220     C                   GOTO      FIND05
097000970617     C                   END                                                    -- 03 --
097100980220      *---------------------------------*
097200970617     C                   ELSE                                                   -- 02 --
097300980220      *---------------------------------*
097400980220      *  COLLI MANCANTI - IDD -
097500980220      *
097600970617     C     VACCCA        IFEQ      '5'                                          -- 03 --
097700970617     C     VACCCA        OREQ      '3'
097800970617     C     VACCCA        OREQ      '7'
097900970319     C     WTREVE        OREQ      'S'
098000980220      *
098100980220      *  Controllo se reso codificato in tab.
098200970121     C                   Z-ADD     1             X
098300970121     C     'IDD'         LOOKUP    C2A(X)                                 32     IDD
098400980220      *
098500970617     C     *IN32         IFEQ      '1'                                          -- 04 --
098600970121     C     §PULAB        IFEQ      'S'
098700970121     C                   MOVEL     CEV(X)        DB9013
098800970121     C                   MOVEL     STS(X)        DB9011
098900970121     C                   ELSE
099000970121     C                   MOVEL     CDV(X)        DB9013
099100970121     C                   MOVEL     STV(X)        DB9011
099200970121     C                   END
099300970121     C                   MOVEL     '7  '         DB2005
099400970121     C                   MOVEL     VACDCM        WDATHM
099500970121     C                   MOVE      VACHMC        WDATHM           12 0
099600970121     C                   MOVEL     WDATHM        DB2380
099700970121     C                   MOVEL     '203'         DB2379
099800980220      *  Scrivo record SD00 e SD05
099900980220     C                   EXSR      WRT0E5
100000980220     C                   GOTO      FIND05
100100970617     C                   END                                                    -- 04 --
100200980220      *---------------------------------*
100300970617     C                   ELSE                                                   -- 03 --
100400980319      *---------------------------------*
100500980319      * Se VACCCA='C' --> ho ricevuto dall'arrivo e devo trasmettere
100600980319      *                   al partner un evento di messa in consegna
100700980319     C     VACCCA        IFEQ      'C'                                          -- 04 --
100800040707      *---------------------------------*
100900040707      *  Prima del MIC        -> STATUS 20/210 SUPERMERCATO
101000040707      *
101100040707      * Non è un evento ma una prticolarità di consegna
101200040721      *  aggiunto anche per appuntamento (Booking ins)
101300081111      *    SOLO SE NON E'STAT MANDATA PRIMA:
101400081112     c                   if        Bolla_variata  = 'N'
101500081112      *
101600081111     c                   if        VACTC1 = 'S' or VACTC2 = 'S' or
101700081111     c                             VACTC1 = 'A' or VACTC2 = 'A'
101800081111     c                   Exsr      STATUS_20_210
101900081111     c                   end
102000081119      *
102100081111     c                   endif
102200081111     c**
102300040707      *---------------------------------*
102400980319     C                   Z-ADD     1             X
102500980319     C     'MIC'         LOOKUP    C2A(X)                                 32
102600980319      *
102700980319     C     *IN32         IFEQ      '1'                                          -- 05 --
102800980319     C     §PULAB        IFEQ      'S'
102900980319     C                   MOVEL     CEV(X)        DB9013
103000980319     C                   MOVEL     STS(X)        DB9011
103100980319     C                   ELSE
103200980319     C                   MOVEL     CDV(X)        DB9013
103300980319     C                   MOVEL     STV(X)        DB9011
103400980319     C                   END
103500980319     C                   MOVEL     '7  '         DB2005
103600980319     C                   MOVEL     VACDCM        WDATHM
103700980319     C                   MOVE      VACHMC        WDATHM           12 0
103800980319     C                   MOVEL     WDATHM        DB2380
103900980319     C                   MOVEL     '203'         DB2379
104000980319      *  Scrivo record SD00 e SD05
104100980319     C                   EXSR      WRT0E5
104200980319     C                   GOTO      FIND05
104300980319     C                   END                                                    -- 05 --
104400980319     C                   ELSE                                                   -- 04 --
104500980220      *---------------------------------*
104600980220      *  CONSEGNA TOTALE
104700970617     C                   Z-ADD     1             X
104800960528     C     'CON'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
104900980220      *
105000980319     C     *IN32         IFEQ      '1'                                          -- 05 --
105100961022     C     §PULAB        IFEQ      'S'
105200961022     C                   MOVEL     CEV(X)        DB9013
105300961022     C                   MOVEL     STS(X)        DB9011
105400961022     C                   ELSE
105500961022     C                   MOVEL     CDV(X)        DB9013
105600961022     C                   MOVEL     STV(X)        DB9011
105700961022     C                   END
105800960528     C                   MOVEL     '7  '         DB2005
105900960528     C                   MOVEL     VACDCM        WDATHM
106000960528     C                   MOVE      VACHMC        WDATHM           12 0
106100960528     C                   MOVEL     WDATHM        DB2380
106200960528     C                   MOVEL     '203'         DB2379
106300980220      *  Scrivo record SD00 e SD05
106400980220     C                   EXSR      WRT0E5
106500980220     C                   GOTO      FIND05
106600980319     C                   END                                                    -- 05 --
106700980220      *
106800980220      *---------------------------------*
106900980319     C                   END                                                    -- 04 --
107000980319     C                   END                                                    -- 03 --
107100970617     C                   END                                                    -- 02 --
107200980220      *---------------------------------*
107300980220      *  La spedizione è stata chiusa per: IDD PARTENZA
107400980220     C                   ELSE
107500980220      *---------------------------------*
107600980220      *
107700980220      * Chiusura bolla x IDD in partenza
107800970617     C     VACCCA        IFEQ      '7'                                          -- 02 --
107900970617     C     VACCCA        OREQ      'P'                                          -- 02 --
108000970617     C                   Z-ADD     1             X
108100980204     C                   Z-ADD     VACAAS        KAA2
108200980204     C                   Z-ADD     VACLNP        KLNP
108300980204     C                   Z-ADD     VACNRS        KNRS
108400980204     C                   Z-ADD     VACNSP        KNSP
108500990810     C                   Z-ADD     WOGGI         KDTV
108600980204     C                   TIME                    KORV
108700990810     C     KEVB1         SETGT     FNEVB05L
108800990810     C     KEVB2         READPE    FNEVB05L                               31
108900980220      *
109000980204     C     EVBCEV        LOOKUP    C2A(X)                                 32     IDD
109100980220      *
109200970617     C     *IN32         IFEQ      '1'                                          -- 03 --
109300970617     C     §PULAB        IFEQ      'S'
109400970617     C                   MOVEL     CEV(X)        DB9013
109500970617     C                   MOVEL     STS(X)        DB9011
109600970617     C                   ELSE
109700970617     C                   MOVEL     CDV(X)        DB9013
109800970617     C                   MOVEL     STV(X)        DB9011
109900970617     C                   END
110000970617     C                   MOVEL     '7  '         DB2005
110100970617     C                   MOVEL     VACDCM        WDATHM
110200970617     C                   MOVE      VACHMC        WDATHM           12 0
110300970617     C                   MOVEL     WDATHM        DB2380
110400970617     C                   MOVEL     '203'         DB2379
110500980220      *  Scrivo record SD00 e SD05
110600980220     C                   EXSR      WRT0E5
110700980220     C                   GOTO      FIND05
110800970617     C                   END                                                    -- 03 --
110900970617     C                   END                                                    -- 02 --
111000980220      *---------------------------------*
111100970617     C                   END
111200980220      *
111300960528     C     FIND05        ENDSR
111400040604      *-------------------------------------------------------------*
111500040604      *? STATUS_20_210      << SUPERMERCATO >>
111600040604      *----------------------------------------------------------------
111700040604     C     STATUS_20_210 Begsr
111800040604      *
111900081112     c                   eval      variata    = 'N'
112000081112      *
112100040607      * Si deve prendere l'ultima variazione del solo tipo record ARBG
112200040607      *  appartenente ad una causale CR, CP, CS, CD
112300040607     C                   Z-ADD     VACAAS        KAAS
112400040607     C                   Z-ADD     VACLNP        KLNP
112500040607     C                   Z-ADD     VACNRS        KNRS
112600040607     C                   Z-ADD     VACNSP        KNSP
112700040607     c     kARF          setgt     fiarbf1c
112800040607     c     kARF          readpe    fiarbf1c
112900040607     c                   dow       not %eof(fiarbf1c)
113000040607      *
113100040607      * solo x tipo record FNARBG00 (variazione bolla)
113200080922     c                   if        *in14 = *on
113300080922      *
113400081111      ** con causale particolare
113500040607     c                   if        arbCVB = 'CR' or
113600040607     c                             arbCVB = 'CS' or
113700080922     c                             arbCVB = 'CD'
113800081119      * Se presente una variazione
113900081119     c                   eval      Bolla_variata  = 'S'
114000080922      *
114100081111      *  Solo se la variazione è avvenuta in giornata viene inviata altrimenti non
114200081111      *   deve essere presa in considerazione:
114300081111     c                   if        arbDTV = wOggi
114400081112     c                   eval      variata        = 'S'
114500040607     c                   leave
114600081111     c                   endIf
114700081111      *
114800040607     c                   end
114900080922      *
115000040607     c                   end
115100080922      *
115200040607     c     kARF          readpe    fiarbf1c
115300040607     c                   enddo
115400040607      *
115500040607     c                   if        variata = 'S'
115600040607      * data e ora variazione
115700040607     C                   MOVEL     arbORV        Wora              4 0
115800040607     C                   MOVE      Wora          WDATHM           12 0
115900040607     C                   MOVEL     arbDTV        WDATHM
116000040607     c                   else
116100040607      * data spedizione
116200040607     C                   MOVEL     vacaas        Wdata             8 0
116300040607     C                   MOVE      vacmgs        Wdata
116400040607     C                   MOVE      '0800'        WDATHM           12 0
116500040607     C                   MOVEL     Wdata         WDATHM
116600040607     c                   end
116700040607      *
116800040607      *  20+210 --> segnala se consegnata a un Supermercato
116900040608     C                   MOVEL     '20 '         DB9011
117000040608     C                   MOVEL     '210'         DB9013
117100040607      *
117200040607     C                   MOVEL     '7  '         DB2005
117300040607     C                   MOVEL     WDATHM        DB2380
117400040607     C                   MOVEL     '203'         DB2379
117500040607      *
117600040607      *  Scrivo record SD00 e SD05 prima dei 2 records della consegna
117700040607     C                   EXSR      WRT0E5
117800040607      *
117900040607      * e nel FTX la data di consegna richiesta se presente
118000040607      *  Pulisco dati DS
118100040607     c                   if        vacdcr <> *zero
118200040607     C                   MOVEL     *BLANKS       WDAT
118300040607     C                   CLEAR                   EDSD15
118400040607      *
118500040607      *   Note generiche ='AAI'
118600040607     C                   MOVEL     'AAI'         DD4451
118700040607     c                   select
118800040607     c                   when      vactcr = *blank
118900040607     C                   eval      DD4440 = 'ON ' +
119000040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
119100040607     c                             %editw(vachcr:'  :  ')
119200040607     c                   when      vactcr = 'P'
119300040607     C                   eval      DD4440 = 'BEFORE ' +
119400040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
119500040607     c                             %editw(vachcr:'  :  ')
119600040607     c                   when      vactcr = 'D'
119700040607     C                   eval      DD4440 = 'AFTER ' +
119800040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
119900040607     c                             %editw(vachcr:'  :  ')
120000040607     c                   endsl
120100040607      *  Scrivo record dati note
120200040607     C                   MOVEL     EDSD15        WDAT
120300040607     C                   Z-ADD     §MSNUM        STAPRG
120400040607     C                   MOVEL     'SD15'        STATPR
120500040607     C                   MOVEL     WDAT          STADAT
120600040607     C                   WRITE     IFTSTA00
120700080422     c                   eval      Righe_Dett = 'S'
120800040607     C                   End
120900040607      *
121000040604     C                   Endsr
121100980220      *-------------------------------------------------------------*
121200980220      *? NUMCMR - Scrivo numero CMR
121300980220      *----------------------------------------------------------------
121400980220     C     NUMCMR        BEGSR
121500980220      *
121600980220     C                   MOVEL     '784'         DB1153
121700980220      *
121800980220     C     WTRRDE        IFEQ      'S'
121900980220      *
122000980220     C     §PTCM1        IFEQ      'S'
122100980220     C     §PTNF1        OREQ      'S'
122200980220     C                   Z-ADD     VACAAS        KAAS
122300980220     C                   Z-ADD     VACLNP        KLNP
122400980220     C                   Z-ADD     VACNRS        KNRS
122500980220     C                   Z-ADD     VACNSP        KNSP
122600980220     C                   MOVEL     'TD1154'      KNMC
122700980220     C                   Z-ADD     1             KNRP1
122800980220     C     KRDE          CHAIN     EDRDE01L                           31
122900980220     C  N31              MOVEL     RDEVAL        DB1154
123000980220     C                   END
123100980220      *
123200980220     C                   END
123300980220      *
123400980220     C                   ENDSR
123500980220      *-------------------------------------------------------------*
123600980220      *? WRT0E5 - Scrittura records d00 e d05
123700980220      *----------------------------------------------------------------
123800980220     C     WRT0E5        BEGSR
123900980220      *  Scrivo tipo record SD00
124000980220     C                   ADD       1             WPROG
124100980220     C                   MOVEL     WPROG         DA1490
124200980220     C                   MOVEL     '9999'        DA1312
124300980220     C                   CLEAR                   WDAT
124400980220     C                   MOVEL     EDSD00        WDAT
124500980220     C                   Z-ADD     §MSNUM        STAPRG
124600980220     C                   MOVEL     'SD00'        STATPR
124700980220     C                   MOVEL     WDAT          STADAT
124800980220     C                   WRITE     IFTSTA00
124900980220      *  Scrivo tipo record SD05
125000980220     C                   MOVEL     EDSD05        WDAT
125100980220     C                   Z-ADD     §MSNUM        STAPRG
125200980220     C                   MOVEL     'SD05'        STATPR
125300980220     C                   MOVEL     WDAT          STADAT
125400980220     C                   WRITE     IFTSTA00
125500980220     C                   MOVEL     'S'           WRTEVE            1
125600080422     c                   eval      Righe_Dett = 'S'
125700980220      *
125800980220     C                   ENDSR
125900980220      *-------------------------------------------------------------*
126000980220      *? WRSD10 - Scrittura record partner - dettaglio
126100980220      *----------------------------------------------------------------
126200980220     C     WRSD10        BEGSR
126300101028      *
126400101028      *  solo chi vuole ricevere la firma del firmatario
126500101028     C                   IF        §PUfirma <>'N'
126600980220      *
126700960712     C                   MOVEL     *BLANKS       WDAT
126800960528     C                   CLEAR                   EDSD10
126900101028      *
127000101028      *  Per Schneider questo segmento NON DEVE essere inviato
127100101028      *
127200980220      *  Controllo se esiste firmatario
127300960528     C                   MOVEL     '1'           KTRC
127400060213     C     Kar4          CHAIN     fiar401L                           31
127500960528     C     *IN31         IFEQ      '0'
127600960528     C                   MOVEL     'AP '         DC3035
127700060213     C                   MOVEL     ar4NOT        DC3036
127800980220      *  Scrivo record dati firmatario
127900960705     C                   MOVEL     EDSD10        WDAT
128000960729     C                   Z-ADD     §MSNUM        STAPRG
128100960705     C                   MOVEL     'SD10'        STATPR
128200960705     C                   MOVEL     WDAT          STADAT
128300960528     C                   WRITE     IFTSTA00
128400080422     c                   eval      Righe_Dett = 'S'
128500960528     C                   END
128600980220      *
128700101028     c                   end
128800101028      *
128900960528     C                   ENDSR
129000980220      *-------------------------------------------------------------*
129100980220      *? WRSD15 - Scrittura record partner - dettaglio
129200980220      *----------------------------------------------------------------
129300960528     C     WRSD15        BEGSR
129400980220      *
129500980220      *  Pulisco dati DS
129600960712     C                   MOVEL     *BLANKS       WDAT
129700960528     C                   CLEAR                   EDSD15
129800980220      *
129900980220      *  Controllo se ci sono delle note di consegna
130000980220     C     VACDCM        IFNE      0                                            -- 01 --
130100980204     C                   MOVEL     *BLANKS       RDEVAL
130200980220      *
130300980220     C     WTRRDE        IFEQ      'S'                                          -- 02 --
130400980204     C                   Z-ADD     VACAAS        KAAS
130500980204     C                   Z-ADD     VACLNP        KLNP
130600980204     C                   Z-ADD     VACNRS        KNRS
130700980204     C                   Z-ADD     VACNSP        KNSP
130800980204     C                   MOVEL     'DD4440'      KNMC
130900980204     C                   Z-ADD     1             KNRP1
131000980204     C     KRDE          CHAIN     EDRDE01L                           31
131100980220      *
131200980220     C     *IN31         IFEQ      '0'                                          -- 03 --
131300980204     C                   MOVEL     'AAI'         DD4451
131400980204     C                   MOVEL     RDEVAL        DD4440
131500980204     C                   DELETE    EDRDE000
131600980220      * controllo se c'è proseguimento
131700980204     C                   Z-ADD     2             KNRP1
131800980204     C     KRDE          CHAIN     EDRDE01L                           31
131900980220      *
132000980220     C     *IN31         IFEQ      '0'                                          -- 04 --
132100980204     C                   MOVE      'AAI'         DD4451
132200980204     C                   MOVE      RDEVAL        DD4440
132300980204     C                   DELETE    EDRDE000
132400980220     C                   END                                                    -- 04 --
132500980220      *
132600980220      *  Scrivo record dati note
132700980204     C                   MOVEL     EDSD15        WDAT
132800980204     C                   Z-ADD     §MSNUM        STAPRG
132900980204     C                   MOVEL     'SD15'        STATPR
133000980204     C                   MOVEL     WDAT          STADAT
133100980204     C                   WRITE     IFTSTA00
133200080422     c                   eval      Righe_Dett = 'S'
133300980220     C                   END                                                    -- 03 --
133400980220     C                   END                                                    -- 02 --
133500980220      *-------------------------------
133600980220     C                   ELSE                                                   -- 01 --
133700980220      *-------------------------------
133800980220      *
133900980220      *  Se  è una giacenza cerco le note giacenza
134000961204     C                   Z-ADD     GCPAGC        KAGC
134100961204     C                   Z-ADD     GCPFGC        KFGC
134200961204     C                   Z-ADD     GCPNGC        KNGC
134300961204     C                   Z-ADD     10            KFAS
134400050309     C     KGNP          CHAIN     tignp01L                           31
134500980220     C     *IN31         IFEQ      '0'                                          -- 02 --
134600961204     C                   MOVEL     'AAI'         DD4451
134700961204     C                   MOVEL     GNPDMC        DD4440
134800961204     C*  Scivo record dati note giacenza
134900961204     C                   MOVEL     EDSD15        WDAT
135000961204     C                   Z-ADD     §MSNUM        STAPRG
135100961204     C                   MOVEL     'SD15'        STATPR
135200961204     C                   MOVEL     WDAT          STADAT
135300961204     C                   WRITE     IFTSTA00
135400080422     c                   eval      Righe_Dett = 'S'
135500980220     C                   END                                                    -- 02 --
135600980220     C                   END                                                    -- 01 --
135700961204     C*
135800960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
135900980220     C     WSD15         IFEQ      'S'                                          -- 01 --
136000960821     C                   MOVEL     *BLANKS       WDAT
136100960821     C                   CLEAR                   EDSD15
136200970318     C***                  MOVEL'AAI'     DD4451
136300970318     C***                  MOVELFTX,1     DD4440
136400970318     C***                  MOVELEDSD15    WDAT
136500970318     C***                  Z-ADD§MSNUM    STAPRG
136600970318     C***                  MOVEL'SD15'    STATPR
136700970318     C***                  MOVELWDAT      STADAT
136800970318     C***                  WRITEIFTSTA00
136900970318     C***                  CLEARWSD15
137000980220     C                   END                                                    -- 01 --
137100980220      *
137200960528     C                   ENDSR
137300960528     C*-------------------------------------------------------------*
137400960528     C*? WRSD20 - Scrittura record partner - dettaglio
137500980219      *----------------------------------------------------------------
137600960528     C     WRSD20        BEGSR
137700980219      *
137800980219      *  Pulisco dati DS
137900960912     C                   MOVEA     *BLANKS       SG1
138000960912     C                   MOVEA     *BLANKS       SG2
138100970318     C                   MOVEA     *BLANKS       SG3
138200970619     C                   MOVEA     *BLANKS       SG4
138300960912     C                   MOVEL     *BLANKS       WDAT
138400960528     C                   CLEAR                   EDSD20
138500960528     C                   MOVEL     '99999'       DE1496
138600960729     C                   MOVE      VACNCL        DE7224
138700980220      *
138800990604      *  Se è stata aperta C.A. elaboro a parte
138900990604     C     WELACA        IFEQ      'S'
139000990604     C                   EXSR      COLCA
139100990604     C                   GOTO      ED20
139200990604     C                   ENDIF
139300990604      *
139400990604      *  Se sto effettuando la consegna totale di una bolla con codice
139500980219      *  consegna anomala = '5' su cui è stata effettuata una consegna
139600980219      *  parziale risfleggo eventuali colli trasmessi e chiusi con IDD
139700980220      *
139800980220     C     BLPCCA        IFEQ      '5'
139900970319     C     WTREVE        OREQ      'S'
140000980220      *
140100970319     C     BLPDCM        IFGT      0
140200970318     C     BLPDCP        ANDGT     0
140300970318     C                   EXSR      LIBIDD
140400970318     C                   END
140500980220      *
140600970319     C                   END
140700980220      *
140800980220      *  Se tabellato devo sempre inviare il dettaglio segnacolli
140900980220      *  Carico tutto
141000970304     C     §PTDET        IFEQ      'S'                                          --- 02 ---
141100960912     C                   EXSR      CARCOL
141200980220     C                   EXSR      WRTCOL
141300960912     C                   END
141400980220      *
141500980223      *  Se tabellato devo inviare dettaglio solo per consegne parziali
141600980223      *  Carico non consegnati
141700970304     C     §PTDET        IFEQ      'N'                                          --- 02 ---
141800970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
141900980223      *
142000980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
142100980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
142200970303     C     BLPCCA        OREQ      '5'
142300970617     C     BLPCCA        OREQ      '3'
142400970619     C     VACCCA        OREQ      '7'
142500970619     C     VACCCA        OREQ      'P'
142600970319     C     WTREVE        OREQ      'S'
142700960912     C                   EXSR      CARCOL
142800980220     C                   EXSR      WRTCOL
142900960912     C                   ELSE
143000980223      *
143100980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
143200970304     C     BLPDCM        IFNE      0
143300970304     C                   EXSR      TUTTI
143400970304     C                   END
143500960912     C                   EXSR      WRT20
143600970304     C                   END                                                    --- 03 ---
143700970304     C                   END                                                    --- 02 ---
143800980223      *
143900980223      *  Se previsto da tabella che non devo mai passare il
144000980223      *  numero dei colli scrivo D20
144100970304     C     §PTDET        IFEQ      'M'                                          --- 02 ---
144200980223      *  Per consegne parziali o IDD loop su dettaglio segnacolli
144300970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
144400980223      *
144500980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
144600980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
144700970304     C     BLPCCA        OREQ      '5'
144800970617     C     BLPCCA        OREQ      '3'
144900970619     C     VACCCA        OREQ      '7'
145000970619     C     VACCCA        OREQ      'P'
145100970319     C     WTREVE        OREQ      'S'
145200970304     C                   EXSR      REDBLT
145300970304     C                   ELSE
145400980223      *
145500980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
145600970304     C     BLPDCM        IFEQ      0
145700970304     C                   EXSR      TUTTI
145800970304     C                   END
145900970304     C                   EXSR      WRT20
146000970304     C                   END                                                    --- 03 ---
146100970304     C                   END                                                    --- 02 ---
146200980223      *
146300990604     C     ED20          ENDSR
146400980220      *----------------------------------------------------------------
146500980220      *? LIBIDD - Controllo se devo sfleggare colli con IDD
146600980220      *----------------------------------------------------------------
146700970318     C     LIBIDD        BEGSR
146800980219      *
146900980219      *  Lettura BLT e caricamento colli
147000970318     C     KBLT          CHAIN     FNBLT01L                           31
147100980219      *  rislego i colli da trasmettete al cliente
147200980220     C     *IN31         DOWEQ     '0'
147300980220      *
147400970318     C     BLTFTR        IFEQ      'C'
147500970318     C     BLTDCM        ANDGT     0
147600970318     C     BLTFL1        ANDEQ     '5'
147700970318     C                   MOVEL     ' '           BLTFTR
147800970318     C                   EXCEPT    AGGTRA
147900970318     C                   END
148000980219      * Lettura successiva
148100970318     C     KBLT          READE     FNBLT01L                               31
148200970318     C                   END
148300980219      *
148400970318     C                   ENDSR
148500980219      *----------------------------------------------------------------
148600980219      *? CARCOL - Routine caricamento colli da dettag.
148700980219      *----------------------------------------------------------------
148800960912     C     CARCOL        BEGSR
148900980219      *
149000050413      *  ---------------------
149100050413      *  Legenda dei contatori:   Y1 = Colli Consegnati
149200050413      *  ----------------------   Y2 = Colli Non Consegnati
149300050413      *                           Y3 = Colli Chiusi con IDD
149400050413      *                           Y4 = Colli Negativi con IDD
149500050413      *
149600960912     C                   Z-ADD     0             Y1                5 0
149700960912     C                   Z-ADD     0             Y2                5 0
149800970318     C                   Z-ADD     0             Y3                5 0
149900970619     C                   Z-ADD     0             Y4                5 0
150000980219      *
150100980220      *  Lettura BLT e caricamento colli nelle schiere
150200960912     C     KBLT          CHAIN     FNBLT01L                           31
150300980223      *
150400980220     C     *IN31         DOWEQ     '0'                                          -- 01 --
150500980219      *
150600980219      *  Considero solo i colli non trasmessi al cliente
150700980220     C     BLTFTR        IFNE      'C'                                          -- 02 --
150800980220      *
150900980220      *-------------------------------------------------
151000980219      *  Controllo se devo caricare colli consegnati
151100960912     C     BLTDCM        IFGT      0
151200970303     C     BLTFL1        ANDEQ     ' '
151300970303     C     §PTDET        ANDEQ     'S'
151400970617     C     BLPCCA        ANDNE     '3'
151500980219      *
151600980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
151700980219     C     §PUBS1        IFEQ      'E'
151800980219     C                   EXSR      CARSGE                                       EUROEXPRESS
151900980219     C                   ELSE
152000980219     C                   EXSR      CARSGN                                       BARTOLINI
152100980219     C                   END
152200980219      *
152300980219     C     WSGN          IFNE      *BLANKS
152400960912     C                   ADD       1             Y1
152500960912     C                   MOVEL     WSGN          SG1(Y1)
152600960912     C                   END
152700980219     C                   END
152800980219      *
152900980220      *-------------------------------------------------
153000980219      *  Controllo se devo caricare colli non consegnati
153100960912     C     BLTDCM        IFEQ      0
153200970304     C     §PTDET        ANDNE     'M'
153300970619     C     VACCCA        ANDNE     '7'
153400970304     C     BLTFL1        OREQ      '5'
153500960913     C     §PTDET        ANDNE     'M'
153600970617     C     BLPDCM        ANDGT     0
153700970617     C     BLTFL1        OREQ      '7'
153800970617     C     §PTDET        ANDNE     'M'
153900970617     C     BLTDCM        ANDGT     0
154000970617     C     VACCCA        ANDNE     'P'
154100970617     C     BLPCCA        OREQ      '3'
154200970617     C     §PTDET        ANDNE     'M'
154300970617     C     BLPDCM        ANDGT     0
154400980219      *
154500980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
154600980219     C     §PUBS1        IFEQ      'E'
154700980219     C                   EXSR      CARSGE                                       EUROEXPRESS
154800980219     C                   ELSE
154900980219     C                   EXSR      CARSGN                                       BARTOLINI
155000980219     C                   END
155100980219      *
155200980219     C     WSGN          IFNE      *BLANKS
155300960912     C                   ADD       1             Y2
155400960912     C                   MOVEL     WSGN          SG2(Y2)
155500960912     C                   END
155600980219     C                   END
155700980220      *
155800980220      *-------------------------------------------------
155900980220      *  Controllo se devo caricare colli chiusi con IDD x evento PID
156000970318     C     BLTFL1        IFEQ      '5'
156100970318     C     §PTDET        ANDNE     'M'
156200970318     C     BLPDCM        ANDEQ     0
156300970617     C     BLTFL1        OREQ      '7'
156400970617     C     §PTDET        ANDNE     'M'
156500970617     C     VACCCA        ANDEQ     'P'
156600980220      *
156700980220      *  Se ha segnacollato il cliente carico in schiera i colli cons.
156800980219     C     §PUBS1        IFEQ      'E'
156900980219     C                   EXSR      CARSGE                                       EUROEXPRESS
157000980219     C                   ELSE
157100980219     C                   EXSR      CARSGN                                       BARTOLINI
157200980219     C                   END
157300980219      *
157400980219     C     WSGN          IFNE      *BLANKS
157500970318     C                   ADD       1             Y3
157600970318     C                   MOVEL     WSGN          SG3(Y3)
157700970318     C                   END
157800980220      *
157900980219     C                   END
158000980220      *
158100980220      *-------------------------------------------------
158200980220      *  Se devo inviare solo colli negativi ed ho IDD li carico in
158300980220      *  SG4 (nel caso non abbia ancora attivato flag FL1)
158400970619     C     §PTDET        IFEQ      'N'
158500970619     C     BLTDCM        ANDNE     0
158600970619     C     BLTFL1        ANDEQ     ' '
158700980220      *
158800970619     C     BLPCCA        IFEQ      '3'
158900970619     C     BLPCCA        OREQ      '5'
159000980220      *
159100970619     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
159200980219     C     §PUBS1        IFEQ      'E'
159300980219     C                   EXSR      CARSGE                                       EUROEXPRESS
159400980219     C                   ELSE
159500980219     C                   EXSR      CARSGN                                       BARTOLINI
159600980219     C                   END
159700980219      *
159800980219     C     WSGN          IFNE      *BLANKS
159900970619     C                   ADD       1             Y4
160000970619     C                   MOVEL     WSGN          SG4(Y4)
160100970619     C                   END
160200980219      *
160300980219     C                   END
160400970620     C                   END
160500980220      *
160600980220     C                   END                                                    -- 02 --
160700980220      *
160800050331      * Se è sempre richiesto il dettaglio Segnacolli sul Partner/Cliente ed
160900050331      * è una spedizione di RESO:
161000050331      * (poichè lo status di RESO viene inviata 2 volte da noi con 2 date differenti:
161100050331      *   la prima volta con la data della bolla di reso generata, la seconda con la
161200050331      *    data di consegna della bolla di reso. Vedi FNVAC00T)
161300050331      * ALLORA SOLO IN QUESTO CASO non si fleggano i segnacolli come trasmessi.
161400050331      *  in maniera da poterli sempre ritrasmettere al Partner/Cliente.
161500050331      * (Caso ARVATO che ragiona esclusivamente sui segnacolli e non sul rif.Sped.)
161600050331     C     §PTDET        ifEQ      'S'
161700050331     C     VACCCA        IFEQ      '2'
161800050331     C     VACCCA        OREQ      '6'
161900050331     c                   goto      salta_UPD
162000050331     c                   end
162100050331     c                   end
162200050331      *
162300980220      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
162400970304     C     BLTDCM        IFNE      0
162500970303     C                   MOVEL     'C'           BLTFTR
162600970303     C                   EXCEPT    AGGTRA
162700970304     C                   END
162800050331      *
162900050331     c     salta_UPD     tag
163000980220      *
163100980220      * Lettura successiva
163200960912     C     KBLT          READE     FNBLT01L                               31
163300980220     C                   END
163400980220      *
163500980220     C                   ENDSR
163600990604      *----------------------------------------------------------------
163700990604      *? COLCA - Routine caricamento colli da C.A.
163800990604      *----------------------------------------------------------------
163900990604     C     COLCA         BEGSR
164000990604      *
164100990604     C                   Z-ADD     *ZEROS        Y4                5 0
164200990604     C                   Z-ADD     *ZEROS        WCOLLI            5 0
164300040715      *
164400040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
164500040715      * SIA COME MAMMA CHE COME FIGLIA
164600040715     c                   clear                   fidn12ds
164700040715     c                   eval      i12aas = kAAS
164800040715     c                   eval      i12lnp = kLNP
164900040715     c                   eval      i12nrs = kNRS
165000040715     c                   eval      i12nsp = kNSP
165100040715     c                   eval      i12fel = 'E'
165200040715     c                   eval      i12fan = 'E'
165300040715     c                   eval      i12fch = 'E'
165400040715     c                   eval      i12fpt = 'E'
165500040715     c                   call      'FIDN12R'
165600040715     c                   parm                    fidn12ds
165700040715     c                   z-add     *zeros        ii                2 0
165800040715      **
165900040715      * se trovata almeno una CA
166000040715     c                   if        o12nca > 0
166100040715     c                   dow       ii <  o12nca
166200040715      **
166300040715     c                   add       1             ii
166400040715     c                   movea     skkey(ii)     dskey
166500040715     C                   z-add     dsaac         kaac
166600040715     C                   z-add     dsfil         kfil
166700040715     C                   z-add     dsnca         knca
166800040715     c     kdct          chain     FNDCT000
166900040715     c                   If        %found(fndct01l)
167000040715     C                   MOVEL     DCTFLO        DDCT01
167100040715     C     §DCTtrpt      ifeq      *BLANKS
167200990604      *
167300040715      ** Leggo testata C.A. per reperire il n. spedizione e per flaggarla come
167400040715      ***  trasmessa
167500040715     C**** KBLP          CHAIN     FNDCT000                           31
167600040715      ****
167700040715     C**** *IN31         DOWEQ     '0'                                          -- 01 --
167800040715     C****               MOVEL     DCTFLO        DDCT01
167900040715      ***Considero solo C.A.: non annullate, non chiuse e non trasmesse
168000040715     C**** DCTATB        IFEQ      *BLANKS                                      -- 02 --
168100040715     C**** DCTDCH        ANDEQ     *ZEROS
168200040715     C**** §DCTtrpt      ANDEQ     *BLANKS
168300990607      *
168400990607      *  Flaggo C.A. come trasmessa
168500020412     C                   MOVEL     'S'           §DCTtrpt
168600990607     C                   MOVEL     DDCT01        DCTFLO
168700990607     C                   EXCEPT    AGGDCT
168800990607      *
168900990604      *  Incremento numero colli
169000990604     C                   ADD       DCTNCN        WCOLLI
169100990604      *
169200990604      *  Controllo se devo caricare i segnacolli
169300990604     C     §PTDET        IFNE      'M'                                          -- 03 --
169400990604      *
169500990604      *  Leggo segnacolli C.A.: non annullati, e non chiusi
169600990604     C     KDCD          CHAIN     FNDCD000                           32
169700990607     C     *IN32         DOWEQ     '0'                                          -- 04 --
169800990604     C     DCDATB        IFEQ      *BLANKS                                      -- 05 --
169900990604     C     DCDDCH        ANDEQ     *ZEROS
170000990604      *
170100990604     C     §PUBS1        IFEQ      'E'
170200990604     C                   EXSR      CARSGE                                       EUROEXPRESS
170300990604     C                   ELSE
170400990604     C                   EXSR      CARSGN                                       BARTOLINI
170500990604     C                   ENDIF
170600990604      *
170700990604     C     WSGN          IFNE      *BLANKS
170800990604     C                   ADD       1             Y4
170900990604     C                   MOVEL     WSGN          SG4(Y4)
171000990604     C                   ENDIF
171100990604      *
171200990604     C                   ENDIF                                                  -- 05 --
171300990607     C     KDCD          READE     FNDCD000                               32
171400990604     C                   ENDDO                                                  -- 04 --
171500040715      *
171600040715     C                   ENDIF                                                  -- 03 --
171700040715      *****
171800040715     C*****              ENDIF                                                  -- 02 --
171900040715      **Lettura successiva
172000040715     C*****KBLP          READE     FNDCT000                               31
172100040715     C*****              ENDDO                                                  -- 01 --
172200040715      *****
172300040715     C                   endif
172400040715     C                   endif
172500040715      *
172600040715     c                   enddo
172700040715     c                   endif
172800040715      *
172900990604      *
173000990607      * SCRIVO FLAT FILE
173100990607     C     §PTDET        IFEQ      'M'                                          -- 01 --
173200990607     C     Y4            OREQ      *ZEROS
173300990607     C                   Z-ADD     WCOLLI        DE7224
173400990607     C                   EXSR      WRT20
173500990607     C                   ELSE
173600990607      *
173700990607     C     Y4            IFNE      *ZEROS                                       -- 02 --
173800050427      *  Nel GID imposta il numero dei Colli Danneggiati
173900050427     C                   Z-ADD     y4            DE7224
174000990607     C                   Z-ADD     *ZEROS        WW
174100990607      *
174200990607     C                   DO        Y4            WE                             -- 03 --
174300990607      *  Scrivo ogni §PUEL2 elementi
174400990607     C     WW            IFEQ      §PUEL2                                       -- 04 --
174500990607     C                   MOVEL     '24'          DE4233
174600990607     C                   EXSR      WRT20
174700990607     C                   MOVEL     *BLANKS       EDSD20
174800990607     C                   Z-ADD     *ZEROS        WW
174900990607     C                   ENDIF                                                  -- 04 --
175000990607     C                   ADD       1             WW
175100990607     C                   MOVEL     SG4(WE)       D20(WW)
175200990607     C                   ENDDO                                                  -- 03 --
175300990607      *
175400990607      *  Se ci sono ancora dei colli scrittura nuovo record
175500990607     C     WW            IFNE      *ZEROS                                       -- 03 --
175600990607     C                   MOVEL     '24'          DE4233
175700990607     C                   EXSR      WRT20
175800990607     C                   MOVEL     *BLANKS       EDSD20
175900990607     C                   ENDIF                                                  -- 03 --
176000990607      *
176100990607     C                   ENDIF                                                  -- 02 --
176200990607     C                   ENDIF                                                  -- 01 --
176300990607      *
176400990604     C                   ENDSR
176500980220      *----------------------------------------------------------------
176600980220      *? WRTCOL - Routine scrittura colli
176700980220      *----------------------------------------------------------------
176800980220     C     WRTCOL        BEGSR
176900980220      *
177000980220      *  Scrivo i colli non consegnati
177100980220     C     Y2            IFGT      0
177200960912     C                   MOVE      Y2            DE7224
177300960912     C                   Z-ADD     0             WW                3 0
177400980220      *
177500980220     C                   DO        Y2            WE                3 0
177600980220      *
177700990607      *  Scrivo ogni §PUEL2 elementi
177800990607     C     WW            IFEQ      §PUEL2
177900990607     C                   MOVEL     '24'          DE4233
178000990607     C                   EXSR      WRT20
178100990607     C                   MOVEL     *BLANKS       EDSD20
178200990607     C                   Z-ADD     0             WW
178300990607     C                   ENDIF
178400990607     C                   ADD       1             WW
178500990607     C                   MOVEL     SG2(WE)       D20(WW)
178600990607     C                   ENDDO
178700990607      *
178800990607      *  Se ci sono ancora dei colli scrittura nuovo record
178900990607     C     WW            IFNE      *ZEROS
179000990607     C                   MOVEL     '24'          DE4233
179100990607     C                   EXSR      WRT20
179200990607     C                   MOVEL     *BLANKS       EDSD20
179300990607     C                   ENDIF
179400990607      *
179500990607     C                   ENDIF
179600980220      *
179700980220      *  Scrivo i colli consegnati
179800980220     C     Y1            IFGT      0                                            -- 01 --
179900980223      *
180000980220      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
180100980220      *  (conse. parziale --> ci sono colli non consegnati)
180200980220      *  (pratica IDD --> ci sono colli persi e colli consegnati)
180300980220     C     Y2            IFGT      0                                            -- 02 --
180400081216     C                   ADD       1             WPROG
180500960912     C                   MOVEL     WPROG         DA1490
180600960912     C                   MOVEL     '9999'        DA1312
180700960912     C                   CLEAR                   WDAT
180800960912     C                   MOVEL     EDSD00        WDAT
180900960912     C                   Z-ADD     §MSNUM        STAPRG
181000960912     C                   MOVEL     'SD00'        STATPR
181100960912     C                   MOVEL     WDAT          STADAT
181200960912     C                   WRITE     IFTSTA00
181300080422     c                   eval      Righe_Dett = 'S'
181400980223      *
181500980220      *  Consegna !!!!!!!
181600960912     C                   Z-ADD     1             X
181700980220     C     'CPA'         LOOKUP    C2A(X)                                 32       ?? CONSE.TOT
181800980220      *
181900980220     C     *IN32         IFEQ      '1'                                          -- 03 --
182000980220     C     §PULAB        IFEQ      'S'                                          -- 04 --
182100961022     C                   MOVEL     CEV(X)        DB9013
182200961022     C                   MOVEL     STS(X)        DB9011
182300980220     C                   ELSE                                                   -- 04 --
182400961022     C                   MOVEL     CDV(X)        DB9013
182500961022     C                   MOVEL     STV(X)        DB9011
182600980220     C                   END                                                    -- 04 --
182700960912     C                   MOVEL     '7  '         DB2005
182800960913     C                   MOVEL     BLPDCP        WDATHM
182900960912     C                   MOVE      VACHMC        WDATHM           12 0
183000960912     C                   MOVEL     WDATHM        DB2380
183100960912     C                   MOVEL     '203'         DB2379
183200980220     C                   END                                                    -- 03 --
183300980223      *
183400980223      *  Scrivo record evento CONSEGNA
183500960912     C                   MOVEL     EDSD05        WDAT
183600960912     C                   Z-ADD     §MSNUM        STAPRG
183700960912     C                   MOVEL     'SD05'        STATPR
183800960912     C                   MOVEL     WDAT          STADAT
183900960912     C                   WRITE     IFTSTA00
184000080422     c                   eval      Righe_Dett = 'S'
184100980223      *
184200980223      *  Pulisco SD20
184300960913     C                   CLEAR                   EDSD20
184400960913     C                   MOVEL     '99999'       DE1496
184500980220     C                   END                                                    -- 02 --
184600980223      *
184700980223      *  Scrivo D20 da SG1
184800960912     C                   MOVE      Y1            DE7224
184900960912     C                   Z-ADD     0             WW                3 0
185000980220     C                   DO        Y1            WE                3 0          -- 02 --
185100980223      *
185200990607      *  Scrivo ogni §PUEL2 elementi
185300990607     C     WW            IFEQ      §PUEL2
185400960913     C                   MOVEL     '24'          DE4233
185500960912     C                   EXSR      WRT20
185600990607     C                   MOVEL     *BLANKS       EDSD20
185700990607     C                   Z-ADD     *ZEROS        WW
185800990607     C                   ENDIF
185900990607     C                   ADD       1             WW
186000990607     C                   MOVEL     SG1(WE)       D20(WW)
186100990607     C                   ENDDO                                                  -- 02 --
186200980223      *
186300990607      *  Se ci sono ancora dei colli scrittura nuovo record
186400990607     C     WW            IFNE      *ZEROS                                       -- 02 --
186500990607     C                   MOVEL     '24'          DE4233
186600990607     C                   EXSR      WRT20
186700960912     C                   MOVEL     *BLANKS       EDSD20
186800990607     C                   ENDIF                                                  -- 02 --
186900980223      *
187000980220     C                   ELSE                                                   -- 01 --
187100980223      *
187200980223      *  Scrivo D20 da SG1
187300970619     C                   Z-ADD     Y1            DE7224
187400970619     C                   Z-ADD     0             WW                3 0
187500980220     C                   DO        Y1            WE                3 0          -- 02 --
187600990607      *
187700990607      *  Scrivo ogni §PUEL2 elementi
187800990607     C     WW            IFEQ      §PUEL2
187900990607     C                   MOVEL     '24'          DE4233
188000990607     C                   EXSR      WRT20
188100990607     C                   MOVEL     *BLANKS       EDSD20
188200990607     C                   Z-ADD     *ZEROS        WW
188300990607     C                   ENDIF
188400990607     C                   ADD       1             WW
188500990607     C                   MOVEL     SG1(WE)       D20(WW)
188600990607     C                   ENDDO                                                  -- 02 --
188700990607      *
188800990607      *  Se ci sono ancora dei colli scrittura nuovo record
188900990607     C     WW            IFNE      *ZEROS                                       -- 02 --
189000990607     C                   MOVEL     '24'          DE4233
189100990607     C                   EXSR      WRT20
189200990607     C                   MOVEL     *BLANKS       EDSD20
189300990607     C                   ENDIF                                                  -- 02 --
189400980223      *
189500980220     C                   END                                                    -- 01 --
189600980223      *
189700980223      *  Controllo se devo scrivere colli chiusi x IDD parziale
189800980220     C     Y3            IFGT      0                                            -- 01 --
189900980223      *
190000980223      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
190100980223      *  (conse.parziale --> ci sono colli non consegnati)
190200980223      *  (pratica IDD --> ci sono colli persi e colli consegnati)
190300980220     C     Y2            IFGT      0                                            -- 02 --
190400081216     C                   ADD       1             WPROG
190500970318     C                   MOVEL     WPROG         DA1490
190600970318     C                   MOVEL     '9999'        DA1312
190700970318     C                   CLEAR                   WDAT
190800970318     C                   MOVEL     EDSD00        WDAT
190900970318     C                   Z-ADD     §MSNUM        STAPRG
191000970318     C                   MOVEL     'SD00'        STATPR
191100970318     C                   MOVEL     WDAT          STADAT
191200970318     C                   WRITE     IFTSTA00
191300080422     c                   eval      Righe_Dett = 'S'
191400980223      *
191500980223      *  Consegna !!!!!!!
191600970318     C                   Z-ADD     1             X
191700970619     C     VACCCA        IFEQ      '7'
191800970617     C     'MAN'         LOOKUP    C2A(X)                                 32
191900990607     C                   ELSE
192000970617     C     'PID'         LOOKUP    C2A(X)                                 32
192100990607     C                   END
192200980220      *
192300990607     C     *IN32         IFEQ      '1'                                          -- 03 --
192400990607     C     §PULAB        IFEQ      'S'
192500970318     C                   MOVEL     CEV(X)        DB9013
192600970318     C                   MOVEL     STS(X)        DB9011
192700990607     C                   ELSE
192800970318     C                   MOVEL     CDV(X)        DB9013
192900970318     C                   MOVEL     STV(X)        DB9011
193000990607     C                   END
193100970318     C                   MOVEL     '7  '         DB2005
193200970318     C                   MOVEL     BLPDCP        WDATHM
193300970318     C                   MOVE      VACHMC        WDATHM           12 0
193400970318     C                   MOVEL     WDATHM        DB2380
193500970318     C                   MOVEL     '203'         DB2379
193600990607     C                   END                                                    -- 03 --
193700980223      *
193800980223      *  Scivo record evento CONSEGNA
193900970318     C                   MOVEL     EDSD05        WDAT
194000970318     C                   Z-ADD     §MSNUM        STAPRG
194100970318     C                   MOVEL     'SD05'        STATPR
194200970318     C                   MOVEL     WDAT          STADAT
194300970318     C                   WRITE     IFTSTA00
194400080422     c                   eval      Righe_Dett = 'S'
194500980223      *
194600980223      *  Pulisco SD20
194700970318     C                   CLEAR                   EDSD20
194800970318     C                   MOVEL     '99999'       DE1496
194900990607     C                   END                                                    -- 02 --
195000980223      *
195100980223      *  Scrivo D20 da SG3
195200970318     C                   MOVE      Y3            DE7224
195300970318     C                   Z-ADD     0             WW                3 0
195400990607     C                   DO        Y3            WE                3 0          -- 02 --
195500990607      *
195600990607      *  Scrivo ogni §PUEL2 elementi
195700990607     C     WW            IFEQ      §PUEL2
195800990607     C                   MOVEL     '24'          DE4233
195900990607     C                   EXSR      WRT20
196000990607     C                   MOVEL     *BLANKS       EDSD20
196100990607     C                   Z-ADD     *ZEROS        WW
196200990607     C                   ENDIF
196300990607     C                   ADD       1             WW
196400990607     C                   MOVEL     SG3(WE)       D20(WW)
196500990607     C                   ENDDO                                                  -- 02 --
196600990607      *
196700990607      *  Se ci sono ancora dei colli scrittura nuovo record
196800990607     C     WW            IFNE      *ZEROS                                       -- 02 --
196900990607     C                   MOVEL     '24'          DE4233
197000990607     C                   EXSR      WRT20
197100990607     C                   MOVEL     *BLANKS       EDSD20
197200990607     C                   ENDIF                                                  -- 02 --
197300980220      *
197400990607     C                   ENDIF                                                  -- 01 --
197500990607      *
197600980220      * Per CCA=5 o CCA=3 e  §PTDET='N' Y2, Y3 = 0 e Y4 > 0 imposto Y4 e SG4
197700990607     C     BLPCCA        IFEQ      '5'                                          -- 01 --
197800970619     C     BLPCCA        OREQ      '3'
197900980220      *
198000990607     C     §PTDET        IFEQ      'N'                                          -- 02 --
198100970619     C     Y2            ANDEQ     0
198200970619     C     Y3            ANDEQ     0
198300970619     C     Y4            ANDNE     0
198400970619     C                   Z-ADD     Y4            DE7224
198500970619     C                   Z-ADD     0             WW                3 0
198600990607     C                   DO        Y4            WE                3 0          -- 03 --
198700990607      *
198800990607      *  Scrivo ogni §PUEL2 elementi
198900990607     C     WW            IFEQ      §PUEL2
199000990607     C                   MOVEL     '24'          DE4233
199100990607     C                   EXSR      WRT20
199200990607     C                   MOVEL     *BLANKS       EDSD20
199300990607     C                   Z-ADD     *ZEROS        WW
199400990607     C                   ENDIF
199500990607     C                   ADD       1             WW
199600990607     C                   MOVEL     SG4(WE)       D20(WW)
199700990607     C                   ENDDO                                                  -- 03 --
199800990607      *
199900990607      *  Se ci sono ancora dei colli scrittura nuovo record
200000990607     C     WW            IFNE      *ZEROS                                       -- 03 --
200100990607     C                   MOVEL     '24'          DE4233
200200990607     C                   EXSR      WRT20
200300990607     C                   MOVEL     *BLANKS       EDSD20
200400990607     C                   ENDIF                                                  -- 03 --
200500990607      *
200600990607     C                   END                                                    -- 02 --
200700990607     C                   END                                                    -- 01 --
200800980223      *
200900960912     C                   ENDSR
201000980223      *----------------------------------------------------------------
201100980223      *? TUTTI - fleggo tutti i record di BLT come trasmessi
201200980223      *----------------------------------------------------------------
201300970304     C     TUTTI         BEGSR
201400980223      *
201500980223      *  Aggiorno come trasmessi i singoli colli
201600970303     C     KBLT          CHAIN     FNBLT01L                           31
201700970303     C     *IN31         DOWEQ     '0'
201800980223      *
201900980223      *  Considero solo i colli non trasmessi al cliente
202000970303     C     BLTFTR        IFNE      'C'
202100980223      *
202200980223      *  Fleggo segnacolli come trasmessi se consegnati
202300970303     C     BLTDCM        IFGT      0
202400980223      *
202500980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
202600970303     C                   MOVEL     'C'           BLTFTR
202700970303     C                   EXCEPT    AGGTRA
202800970303     C                   END
202900970304     C                   END
203000980223      *
203100980223      * Lettura successiva
203200970303     C     KBLT          READE     FNBLT01L                               31
203300970303     C                   END
203400980223      *
203500970304     C                   ENDSR
203600980223      *----------------------------------------------------------------
203700980223      *? REDBLT - fleggo tutti i record di BLT come trasmessi
203800980223      *----------------------------------------------------------------
203900970304     C     REDBLT        BEGSR
204000980223      *
204100970304     C                   Z-ADD     0             WCOLCO            5 0
204200970304     C                   Z-ADD     0             WCOLID            5 0
204300980223      *
204400980223      *  Aggiorno come trasmessi i singoli colli
204500970304     C     KBLT          CHAIN     FNBLT01L                           31
204600970304     C     *IN31         DOWEQ     '0'
204700980223      *
204800980223      *  Considero solo i colli non trasmessi al cliente
204900970304     C     BLTFTR        IFNE      'C'
205000980223      *
205100980223      *  Fleggo segnacolli come trasmessi se consegnati
205200970304     C     BLTDCM        IFGT      0
205300970320     C     BLTFL1        OREQ      '5'
205400970617     C     BLTFL1        OREQ      '7'
205500970617     C     BLPCCA        OREQ      '3'
205600980223      *
205700980223      *  Se chiusi con pratica IDD aggiungo 1 al relativo nr.colli
205800980223      *  altrimenti CONTROLLO SE sono colli consegnati
205900970304     C     BLTFL1        IFEQ      '5'
206000970617     C     BLTFL1        OREQ      '7'
206100970617     C     BLPCCA        OREQ      '3'
206200970304     C                   ADD       1             WCOLID
206300970304     C                   ELSE
206400970304     C                   ADD       1             WCOLCO
206500970320     C                   END
206600980223      *
206700980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
206800970304     C                   MOVEL     'C'           BLTFTR
206900970304     C                   EXCEPT    AGGTRA
207000970304     C                   END
207100970304     C                   END
207200980223      *
207300980223      * Lettura successiva
207400970304     C     KBLT          READE     FNBLT01L                               31
207500970304     C                   END
207600980223      *
207700980223      * Se ci sono dei colli chiusi con pratica IDD scrivo TD20 per loro
207800970304     C     WCOLID        IFNE      0
207900970304     C                   MOVE      WCOLID        DE7224
208000970304     C                   EXSR      WRT20
208100980223      *
208200980223      * Se ci sono anche dei colli consegnati normalmente riscrivo SD00 e SD05
208300970304     C     WCOLCO        IFNE      0
208400081216     C                   ADD       1             WPROG
208500970304     C                   MOVEL     WPROG         DA1490
208600970304     C                   MOVEL     '9999'        DA1312
208700970304     C                   CLEAR                   WDAT
208800970304     C                   MOVEL     EDSD00        WDAT
208900970304     C                   Z-ADD     §MSNUM        STAPRG
209000970304     C                   MOVEL     'SD00'        STATPR
209100970304     C                   MOVEL     WDAT          STADAT
209200970304     C                   WRITE     IFTSTA00
209300080422     c                   eval      Righe_Dett = 'S'
209400980223      *
209500980223      * consegna parziale
209600970304     C                   Z-ADD     1             X
209700970304     C     'CPA'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
209800970304     C     *IN32         IFEQ      '1'
209900970304     C     §PULAB        IFEQ      'S'
210000970304     C                   MOVEL     CEV(X)        DB9013
210100970304     C                   MOVEL     STS(X)        DB9011
210200970304     C                   ELSE
210300970304     C                   MOVEL     CDV(X)        DB9013
210400970304     C                   MOVEL     STV(X)        DB9011
210500970304     C                   END
210600970304     C                   MOVEL     '7  '         DB2005
210700970304     C                   MOVEL     BLPDCP        WDATHM
210800970304     C                   MOVE      VACHMC        WDATHM           12 0
210900970304     C                   MOVEL     WDATHM        DB2380
211000970304     C                   MOVEL     '203'         DB2379
211100970304     C                   END
211200980223      *
211300980223      *  Scivo record evento CONSEGNA
211400970304     C                   MOVEL     EDSD05        WDAT
211500970304     C                   Z-ADD     §MSNUM        STAPRG
211600970304     C                   MOVEL     'SD05'        STATPR
211700970304     C                   MOVEL     WDAT          STADAT
211800970304     C                   WRITE     IFTSTA00
211900080422     c                   eval      Righe_Dett = 'S'
212000980223      *
212100980223      *  Pulisco SD20
212200970304     C                   CLEAR                   EDSD20
212300970304     C                   MOVEL     '99999'       DE1496
212400970304     C                   END
212500970304     C                   END
212600980223      *
212700980223      * Scrivo colli consegnati regolarmente
212800970304     C     WCOLCO        IFNE      0
212900970304     C                   MOVE      WCOLCO        DE7224
213000970304     C                   EXSR      WRT20
213100970304     C                   END
213200980223      *
213300970320     C                   ENDSR
213400980220      *----------------------------------------------------------------
213500980220      *? CHKBLT - Controllo se ci sono colli da trasmettere
213600980220      *----------------------------------------------------------------
213700970320     C     CHKBLT        BEGSR
213800980220      *
213900980220      *  Aggiorno come trasmessi i singoli colli
214000970320     C                   MOVEL     'N'           WTRCOL            1
214100970320     C     KBLT          CHAIN     FNBLT01L                           31
214200980220      *
214300970320     C     *IN31         DOWEQ     '0'
214400970320     C     WTRCOL        ANDEQ     'N'
214500980220      *  Considero solo i colli non trasmessi al cliente
214600970320     C     BLTFTR        IFNE      'C'
214700980220      *  Fleggo segnacolli come trasmessi se consegnati
214800970320     C     BLTDCM        IFGT      0
214900970320     C     BLTFL1        OREQ      '5'
215000970320     C                   MOVEL     'S'           WTRCOL
215100970320     C                   END
215200970320     C                   END
215300980220      * Lettura successiva
215400970320     C     KBLT          READE     FNBLT01L                               31
215500970320     C                   END
215600980220      *
215700970320     C                   ENDSR
215800980223      *----------------------------------------------------------------
215900980223      *? WRT20 - scrivo record d20
216000980223      *----------------------------------------------------------------
216100970304     C     WRT20         BEGSR
216200980223      *  Scrivo record SD20
216300960912     C                   MOVEL     EDSD20        WDAT
216400960912     C                   MOVEL     'SD20'        STATPR
216500960912     C                   Z-ADD     §MSNUM        STAPRG
216600960912     C                   MOVEL     WDAT          STADAT
216700960912     C                   WRITE     IFTSTA00
216800080422     c                   eval      Righe_Dett = 'S'
216900980223      *
217000960912     C                   ENDSR
217100980223      *----------------------------------------------------------------
217200980223      *? CARSGE - caricamento segnacollo EUROEXPRESS
217300980223      *----------------------------------------------------------------
217400980219     C     CARSGE        BEGSR
217500980219      *
217600990607     C     VACCCA        IFEQ      'A'
217700990607     C                   Z-ADD     DCDFLS        KFLS
217800990607     C                   Z-ADD     DCDLNA        KLNA
217900990607     C                   Z-ADD     DCDNRS        KNRS
218000990607     C                   Z-ADD     DCDNSC        KNSC
218100990607     C                   ELSE
218200990607     C                   Z-ADD     BLTFLS        KFLS
218300990607     C                   Z-ADD     BLTLNA        KLNA
218400990607     C                   Z-ADD     BLTNRS        KNRS
218500990607     C                   Z-ADD     BLTNSC        KNSC
218600990607     C                   ENDIF
218700050120      *
218800050120     C                   MOVEL     'E'           KTRC
218900050120      *
219000050120      * Controlla se non è Euroexpress ma un cliente normale come Disk C
219100050120     C     kfls          chain     AZORG01L
219200050120     C                   clear                   OG143
219300050120     c                   if        %Found(AZORG01L)
219400050120     C                   movel     ORGDE3        OG143
219500050120     C     §OGntw        IFne      'EEX'
219600050120     C                   z-add     1             KKUT
219700050120     C                   movel     '1B'          KCOD
219800050120     C                   movel(p)  §ptCTM        KKEY
219900050120     C     KTAB2         chain     tabel00f
220000050120     c                   if        %Found(tabel00f)
220100050120     c                   movel     tbluni        ds1B
220200050120      * se si tratta di un Disk "C" non Euroexpress --> Cliente
220300050120     c                   if        §1BF12 <> 'N'
220400050120     C                   MOVEL     'C'           KTRC
220500050120     c                   end
220600050120     c                   end
220700050120     c                   end
220800050120     c                   end
220900050120      *
221000980219     C                   MOVEL     *BLANKS       WSGN
221100980219      *
221200051017     C     KARS          CHAIN     FIARS000                           33
221300051017     C  N33              MOVEL     ARSNOT        WSGN
221400980219      *
221500980219     C                   ENDSR
221600980223      *----------------------------------------------------------------
221700980223      *? CARSGN - caricamento segnacollo
221800980223      *----------------------------------------------------------------
221900980219     C     CARSGN        BEGSR
222000980219      *
222100980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
222200960912     C                   MOVEA     *BLANKS       C35
222300960912     C                   MOVEL     *BLANKS       WSGN             35
222400980219      *  Compongo lnp se la devo passare
222500960912     C     §PULP2        IFNE      0
222600960912     C                   Z-ADD     §PULP2        WA                3 0
222700961029     C                   MOVEL     BLPFLS        WLNP              3
222800960912     C                   MOVEA     WLNP          C35(WA)
222900960912     C                   END
223000980219      *  Compongo lna se la devo passare
223100960912     C     §PULA2        IFNE      0
223200990607     C                   Z-ADD     §PULA2        WA
223300960912     C                   MOVEL     BLPLNA        WLNA              3
223400960912     C                   MOVEA     WLNA          C35(WA)
223500960912     C                   END
223600980219      *  Compongo nrs se lo devo passare
223700960912     C     §PUNR2        IFNE      0
223800990607     C                   Z-ADD     §PUNR2        WA
223900960912     C                   MOVEL     BLPNRS        WNRS              2
224000960912     C                   MOVEA     WNRS          C35(WA)
224100960912     C                   END
224200980219      *  Compongo nsc se la devo passare
224300960912     C     §PUSG2        IFNE      0
224400990607     C                   Z-ADD     §PUSG2        WA
224500990607     C     VACCCA        IFEQ      'A'
224600990604     C                   MOVEL     DCDNSC        WNSC              7
224700990604     C                   ELSE
224800990604     C                   MOVEL     BLTNSC        WNSC
224900990604     C                   END
225000960912     C                   MOVEA     WNSC          C35(WA)
225100960912     C                   END
225200980219      *  Compongo znc se la devo passare
225300960912     C     §PUZN2        IFNE      0
225400990607     C                   Z-ADD     §PUZN2        WA
225500990607     C                   MOVEL     BLPZNC        WZNC              2
225600960912     C                   MOVEA     WZNC          C35(WA)
225700960912     C                   END
225800980219      *  metto C35 in WSGN
225900960912     C                   MOVEA     C35           WSGN
226000980227      *
226100960730     C                   ENDSR
226200960927     C*----------------------------------------------------------------
226300960927     C*? CARNUM - CARICAMENTO NUMERATORE
226400960927     C*----------------------------------------------------------------
226500960927     C     CARNUM        BEGSR
226600960927     C*
226700960927     C                   Z-ADD     0             WPROG             4 0
226800960927     C                   Z-ADD     0             X                 3 0
226900960927     C                   Z-ADD     1             §MSNUM
227000960927     C                   MOVEL     'MS'          KCOD1
227100960927     C                   MOVEL     *BLANKS       KKEY1
227200960927     C                   MOVEL     '784'         KKEY1
227300960927     C     KTABE         CHAIN     EDTAB01L                           30
227400960927     C     *IN30         IFEQ      '0'
227500960927     C     TABFLG        ANDEQ     *BLANKS
227600020412     C                   MOVEL     TABUNI        EDIDSMS
227700960927     C                   ADD       1             §MSNUM
227800061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
227900061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
228000061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
228100061219     c                   move      §MSNUM        campo_4n          4 0
228200061219     c                   if        campo_4n = 0
228300061219     c                   add       1             §MSNUM
228400061219     c                   end
228500960927     C                   Z-ADD     WOGGI         §MSDAT
228600020412     C                   MOVEL     EDIDSMS       TABUNI
228700960927     C                   UPDATE    EDTAB000
228800960927     C                   END
228900980223      *
229000960927     C                   ENDSR
229100980223      *----------------------------------------------------------------
229200980223      *? *INZSR - OPERAZIONI INIZIALI
229300980223      *----------------------------------------------------------------
229400960528     C     *INZSR        BEGSR
229500980223      *
229600080418      *  *entry vecchia gestione con il FNVAC multimebro
229700080418     C**** *ENTRY        PLIST
229800080418     C*******            PARM                    MBR              10            da FNVAC
229900080418     C*******            PARM                    TPR               1            Europolitan
230000080418     C*******            PARM                    TerPar            3            x Filiali
230100080418      *
230200080418      *?  Deve leggere il nuovo TIVGD x generare EDI e fare il Download...  ?
230300080418     C     *ENTRY        PLIST
230400080418     C                   PARM                    CLIENTE           8            "0xxxyyyy"
230500080418     C                   PARM                    Tipo_File         2            "VC"
230600080418     C                   PARM                    Tipo_Scarico      2            "EW"
230700080418     C                   PARM                    Righe_Dett        1            "N"
230800080418     c                   eval      Righe_Dett = 'N'
230900080418      *
231000980223      * Definisco chiavi di accesso
231100080418     C     KEY_VGD01     KLIST
231200080418     C                   KFLD                    Tipo_File                      "VC"
231300080418     C                   KFLD                    CLIENTE                        "0xxxyyyy"
231400080418     C                   KFLD                    Tipo_Scarico                   "EW"
231500080418      *
231600960726     C     KTABE         KLIST
231700960726     C                   KFLD                    KCOD1
231800960726     C                   KFLD                    KKEY1
231900960528     C     KBLP          KLIST
232000960528     C                   KFLD                    KAAS
232100960528     C                   KFLD                    KLNP
232200960528     C                   KFLD                    KNRS
232300960528     C                   KFLD                    KNSP
232400990604     C     KDCD          KLIST
232500990604     C                   KFLD                    DCTAAC
232600990604     C                   KFLD                    DCTFIL
232700990604     C                   KFLD                    DCTNCA
232800960528     C     KGCP          KLIST
232900960528     C                   KFLD                    KAAS
233000960528     C                   KFLD                    KLNP
233100960528     C                   KFLD                    KNRS
233200960528     C                   KFLD                    KNSP
233300960528     C                   KFLD                    KFRG
233400050922     C     KGCp_1        KLIST
233500050922     C                   KFLD                    KAAS
233600050922     C                   KFLD                    KLNP
233700050922     C                   KFLD                    KNRS
233800050922     C                   KFLD                    KNSP
233900961204     C     KGNP          KLIST
234000961204     C                   KFLD                    KAGC
234100961204     C                   KFLD                    KFGC
234200961204     C                   KFLD                    KNGC
234300961204     C                   KFLD                    KFRG
234400961204     C                   KFLD                    KFAS
234500960528     C     KBLT          KLIST
234600960528     C                   KFLD                    KAAS
234700960528     C                   KFLD                    KLNP
234800960528     C                   KFLD                    KNRS
234900960528     C                   KFLD                    KNSP
235000970319     C     KLBL1         KLIST
235100970319     C                   KFLD                    KAAN
235200970319     C                   KFLD                    KLPN
235300970319     C                   KFLD                    KNRN
235400970319     C                   KFLD                    KNSN
235500970319     C     KLBL2         KLIST
235600970319     C                   KFLD                    KAAP
235700970319     C                   KFLD                    KLPP
235800970319     C                   KFLD                    KNRP
235900970319     C                   KFLD                    KNSP
236000060213     C     Kar4          KLIST
236100960528     C                   KFLD                    KAAS
236200960528     C                   KFLD                    KLNP
236300960528     C                   KFLD                    KNRS
236400960528     C                   KFLD                    KNSP
236500960528     C                   KFLD                    KTRC
236600960628     C     KRDE          KLIST
236700960628     C                   KFLD                    KAAS
236800960628     C                   KFLD                    KLNP
236900960628     C                   KFLD                    KNRS
237000960628     C                   KFLD                    KNSP
237100960628     C                   KFLD                    KNMC
237200970319     C                   KFLD                    KNRP1
237300961029     C     KTAB1         KLIST
237400961029     C                   KFLD                    KKUT
237500961029     C                   KFLD                    KCOD
237600970319     C     KTAB2         KLIST
237700970319     C                   KFLD                    KKUT
237800970319     C                   KFLD                    KCOD
237900970319     C                   KFLD                    KKEY
238000970319     C     KEVB          KLIST
238100970319     C                   KFLD                    KAA2
238200970319     C                   KFLD                    KLNP
238300970319     C                   KFLD                    KNRS
238400970319     C                   KFLD                    KNSP
238500970319     C                   KFLD                    KCEV
238600980204     C     KEVB1         KLIST
238700980204     C                   KFLD                    KAA2
238800980204     C                   KFLD                    KLNP
238900980204     C                   KFLD                    KNRS
239000980204     C                   KFLD                    KNSP
239100980204     C                   KFLD                    KDTV
239200980204     C                   KFLD                    KORV
239300980204     C     KEVB2         KLIST
239400980204     C                   KFLD                    KAA2
239500980204     C                   KFLD                    KLNP
239600980204     C                   KFLD                    KNRS
239700980204     C                   KFLD                    KNSP
239800051017     C     KARS          KLIST
239900980219     C                   KFLD                    KFLS
240000980219     C                   KFLD                    KLNA
240100980219     C                   KFLD                    KNRS
240200980219     C                   KFLD                    KNSC
240300980219     C                   KFLD                    KTRC
240400040607     C     KARF          klist
240500040607     C                   kfld                    KAAS
240600040607     C                   kfld                    KLNP
240700040607     C                   kfld                    KNRS
240800040607     C                   kfld                    KNSP
240900040715
241000040715     C     Kdct          klist
241100040715     C                   KFLD                    kaac
241200040715     C                   KFLD                    kfil
241300040715     C                   KFLD                    knca
241400040715      *
241500980223      * Definizione campi di work
241600980219     C     *LIKE         DEFINE    BLTNSC        KNSC
241700980219     C     *LIKE         DEFINE    BLTLNA        KLNA
241800051017     C     *LIKE         DEFINE    ARSFLS        KFLS
241900980219     C     *LIKE         DEFINE    TBLKUT        KKUT
242000961029     C     *LIKE         DEFINE    TBLCOD        KCOD
242100970319     C     *LIKE         DEFINE    TBLKEY        KKEY
242200961029     C     *LIKE         DEFINE    TABCOD        KCOD1
242300960726     C     *LIKE         DEFINE    TABKEY        KKEY1
242400960528     C     *LIKE         DEFINE    BLPAAS        KAAS
242500960528     C     *LIKE         DEFINE    BLPLNP        KLNP
242600960528     C     *LIKE         DEFINE    BLPNRS        KNRS
242700960528     C     *LIKE         DEFINE    BLPNSP        KNSP
242800060213     C     *LIKE         DEFINE    ar4TRC        KTRC
242900960528     C     *LIKE         DEFINE    GCPFRG        KFRG
243000960628     C     *LIKE         DEFINE    RDENMC        KNMC
243100970319     C     *LIKE         DEFINE    RDENRP        KNRP1
243200961204     C     *LIKE         DEFINE    GNPAGC        KAGC
243300961204     C     *LIKE         DEFINE    GNPFGC        KFGC
243400961204     C     *LIKE         DEFINE    GNPNGC        KNGC
243500961204     C     *LIKE         DEFINE    GNPFAS        KFAS
243600970319     C     *LIKE         DEFINE    LBLAAN        KAAN
243700970319     C     *LIKE         DEFINE    LBLLPN        KLPN
243800970319     C     *LIKE         DEFINE    LBLNRN        KNRN
243900970319     C     *LIKE         DEFINE    LBLNSN        KNSN
244000970319     C     *LIKE         DEFINE    LBLAAP        KAAP
244100970319     C     *LIKE         DEFINE    LBLLPP        KLPP
244200970319     C     *LIKE         DEFINE    LBLNRP        KNRP
244300970319     C     *LIKE         DEFINE    EVBAAS        KAA2
244400970319     C     *LIKE         DEFINE    EVBCEV        KCEV
244500980204     C     *LIKE         DEFINE    EVBDTV        KDTV
244600980204     C     *LIKE         DEFINE    EVBORV        KORV
244700980223      *
244800980223      * Recupero codice AS
244900960528     C                   Z-ADD     1             CODUT             1 0
245000960705     C                   CALL      'X§PARUT'
245100020412     C                   PARM                    UT§DSE0F
245200960528     C                   MOVEL     REC80         CNCR80
245300980223      *
245400980223      * Recupero data e ora x messaggio
245500960528     C                   TIME                    WHHDAT           14 0
245600960708     C                   MOVE      WHHDAT        G02DAT
245700960528     C                   MOVE      WHHDAT        DATA              6 0
245800960528     C                   MOVEL     *BLANK        G02ERR
245900960528     C                   CALL      'XSRDA8'
246000960528     C                   PARM                    WLBDA8
246100960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
246200960708     C                   MOVEL     WHHDAT        WHHMM             4 0
246300960528     C                   MOVE      WHHDAT        WANNO             2 0
246400100304     C                   Z-ADD     woggi         WOGGIymd          6 0           UDATE
246500980629      * ---------------------------------------------
246600980629      *  CARICO TABELLE
246700980629      * ---------------------------------------------
246800980223      * Caricamento Tabella eventi con IDD
246900050909      *    o tabella Giacenze/Lasciati Avviso
247000970319     C                   Z-ADD     0             X
247100050909     C                   Z-ADD     0             Xj                3 0
247200050922     C                   Z-ADD     0             Xa                3 0
247300970319     C                   Z-ADD     1             KKUT
247400970319     C                   MOVEL     '2A'          KCOD
247500970319     C                   MOVEA     *BLANKS       IDD
247600050922     C                   MOVEA     *BLANKS       GiaC
247700050926     C                   MOVEA     *BLANKS       LAvv
247800970319     C     KTAB1         CHAIN     TABEL00F                           30
247900970319     C     *IN30         DOWEQ     '0'
248000970319     C     TBLFLG        IFEQ      *BLANKS
248100970319     C                   MOVEL     TBLUNI        DS2A
248200050909      * con IDD
248300970319     C     §2AIDD        IFNE      *BLANKS
248400970319     C                   ADD       1             X
248500970319     C                   MOVEL     TBLKEY        IDD(X)
248600970319     C                   END
248700050909      *
248800050922      * Cod. Giacenza
248900050926     C                   if        §2AFTC = 'G'
249000050909     C                   add       1             Xj
249100050922     C                   MOVEL     TBLKEY        GiaC(Xj)
249200050909     C                   end
249300050922      * Lasciato Avviso
249400050926     C                   if        §2AFTC = 'A'
249500050922     C                   add       1             Xa
249600050926     C                   MOVEL     TBLKEY        LAvv(Xa)
249700050922     C                   end
249800050922      *
249900970319     C                   END
250000970319     C     KTAB1         READE     TABEL00F                               30
250100970319     C                   END
250200980629      *
250300020418      * Caricamento £1
250400030926     c***********        clear                   trul06ds
250500030926     c***********        eval      d06cod = '£1'
250600030924     c*******            movel     simfel        d06key
250700030926     c***********        movel     TerPar        d06key
250800030926     c***********        movel     trul06ds      kpjbu
250900030926     c***********        call      'TRUL06R'
251000030926     c***********        parm                    kpjba
251100030926     c***********        movel     kpjbu         trul06ds
251200030926     c***********        movel     lin           £1
251300030926      *
251400030926     ** carico la schiera dei P.O presenti sull AS
251500030926     c                   clear                   tibs56ds
251600030926     c                   eval      i56ppo = simfel
251700030926     c                   eval      i56mod = 'POS'
251800030926     c                   call      'TIBS56R'
251900030926     c                   parm                    tibs56ds
252000030926     C                   MOVEA     SKI           Pou
252100980629      *
252200980223      * Caricamento Tabella Partner esteri
252300960528     C                   Z-ADD     0             X                 3 0
252400960528     C                   MOVEL     'PT'          TABCOD
252500960528     C     TABCOD        CHAIN     EDTAB01L                           30
252600960528     C     *IN30         DOWEQ     '0'
252700960528     C     TABFLG        IFEQ      *BLANKS
252800960528     C                   ADD       1             X
252900960528     C                   MOVEL     TABKEY        CPT(X)
253000961023     C                   MOVEL     TABKEY        CPU(X)
253100961021     C                   MOVE      '    '        CPT(X)
253200960528     C                   MOVEL     TABUNI        KSC(X)
253300960708     C                   MOVEL     TABUNI        DPT(X)
253400960528     C                   END
253500960528     C     TABCOD        READE     EDTAB01L                               30
253600960528     C                   END
253700980223      *
253800091117      * Deve comunque controllare che i codici caricati dalla PT
253900091117      *  non abbiano un altro codice di riferimento sulla 3K con cui
254000091117      *  impostare il membro.
254100091117     c                   do        x             xx
254200091117     C                   Z-ADD     1             KKUT
254300091117     C                   MOVEL     '3K'          KCOD
254400091117     C                   movel(p)  *zeros        KKEY
254500091117     C                   move      KSC(xx)       KKEY
254600091117     C     KTAB2         chain     tabel00f
254700091117     c                   if        %Found(tabel00f)
254800091117      *
254900091117     c                   movel     tbluni        ds3K
255000091117     c                   if        §3Kcks <> KSC(xx)
255100091117      *  sostituisce il codice con quello del membro
255200091117     c                   move      §3Kcks        Ksc(xx)
255300091117      *
255400091117     c                   end
255500091117     c                   end
255600091117     C                   END
255700980629      *
255800980629      *  In base al codice del PARTNER reperisco identificativo
255900080418     C******             MOVEL     MBR           WMBR              8
256000080418     C******             MOVE      WMBR          WCCM              7 0
256100980629      *
256200080418     C                   MOVE      Cliente       WCCM              7 0
256300020221     C                   Z-ADD     1             XX                3 0
256400980629     C     WCCM          LOOKUP    KSC(XX)                                32
256500020916      *
256600070503      *?  Se non trova la "PT" non deve proseguire   ?
256700070503     C     *IN32         IFEQ      '0'
256800070503     c                   move      'S'           wFINE
256900070503     c                   leaveSR
257000070503     c                   end
257100070503      *
257200980629     C     *IN32         IFEQ      '1'
257300980629      *
257400020412     C                   MOVEL     DPT(XX)       EDIDSPT
257500980629      *
257600980629     C                   MOVEL     'PU'          KCOD1
257700980707     C                   MOVEL     CPU(XX)       KKEY1
257800980707     C     KTABE         CHAIN     EDTAB01L                           30
257900980629      *
258000980629     C     *IN30         IFEQ      '0'
258100980629     C     TABFLG        ANDEQ     *BLANKS
258200020412     C                   MOVEL     TABUNI        EDIDSPU
258300980629     C                   END
258400980629      *
258500980629      * Caricamento Tabella codici eventi. Se richesta Lista Labarta e codici
258600980629      *   Labarta sono Blanks non carico rcd, lo stesso per Lista VGL.
258700980629      *
258800980629     C                   Z-ADD     0             X                 3 0
258900980629     C                   MOVEL     '2A'          TABCOD
259000980629      *
259100980629     C     TABCOD        CHAIN     EDTAB01L                           30
259200980629     C     *IN30         DOWEQ     '0'
259300020412     C                   MOVEL     TABUNI        EDIDS2A
259400980629      *
259500980629     C                   SELECT
259600980629      *
259700980629     C     TABFLG        WHENNE    *BLANKS
259800980629      *
259900980629     C     §PULAB        WHENEQ    'S'
260000980629     C     §2ASTS        ANDEQ     *BLANKS
260100980629     C     §2ACEV        ANDEQ     *BLANKS
260200980629      *
260300980629     C     §PULAB        WHENEQ    *BLANKS
260400980629     C     §2ASTV        ANDEQ     *BLANKS
260500980629     C     §2ACDV        ANDEQ     *BLANKS
260600980629      *
260700980629     C                   OTHER
260800980629     C                   ADD       1             X
260900980629     C                   MOVEL     TABKEY        C2A(X)
261000980629     C                   MOVEL     §2ACEV        CEV(X)
261100980629     C                   MOVEL     §2ASTS        STS(X)
261200980629     C                   MOVEL     §2ACDV        CDV(X)
261300980629     C                   MOVEL     §2ASTV        STV(X)
261400980629     C                   ENDSL
261500980629      *
261600980629     C     TABCOD        READE     EDTAB01L                               30
261700980629     C                   END
261800980629      *
261900980629     C                   END
262000980629      *---------------------------------------
262100980629      *
262200980223      * Caricamento NUMERATORE
262300960927     C                   EXSR      CARNUM
262400980223      * finta read x IFTSTA
262500960528     C   01
262600960528     CANN01              READ      EDIFTSTA                               01
262700980223      *
262800980223      * Controllo se esiste EDRDE
262900980126     C                   Z-ADD     45            LUNG             15 5
263000980126     C                   MOVEL     *BLANKS       COMMAN
263100980126     C                   MOVEA     CMD(1)        COMMAN           80
263200980126     C                   CALL      'QCMDEXC'                            21
263300980126     C                   PARM                    COMMAN
263400980126     C                   PARM                    LUNG
263500980126     C     *IN21         IFEQ      '0'
263600980126     C                   OPEN      EDRDE01L
263700980126     C                   MOVEL     'S'           WTRRDE            1
263800980126     C                   END
263900980223      *
264000960528     C                   ENDSR
264100980223      *-----------------------------------------------------*
264200980223      *   EXCPT   x aggiornamento flag trasmissione BLT     *
264300980223      *-----------------------------------------------------*
264400970303     OFNBLT000  E            AGGTRA
264500970303     O                       BLTFTR
264600990607      *-----------------------------------------------------*
264700990607      *   EXCPT   x aggiornamento flag trasmissione DCT     *
264800990607      *-----------------------------------------------------*
264900990607     OFNDCT000  E            AGGDCT
265000990607     O                       DCTFLO
265100980126**
265200980126CHKOBJ OBJ(EDRDE01L) OBJTYPE(*FILE)
