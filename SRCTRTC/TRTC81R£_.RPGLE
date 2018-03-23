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
126300980220      *
126400960712     C                   MOVEL     *BLANKS       WDAT
126500960528     C                   CLEAR                   EDSD10
126600980220      *
126700980220      *  Controllo se esiste firmatario
126800960528     C                   MOVEL     '1'           KTRC
126900060213     C     Kar4          CHAIN     fiar401L                           31
127000960528     C     *IN31         IFEQ      '0'
127100960528     C                   MOVEL     'AP '         DC3035
127200060213     C                   MOVEL     ar4NOT        DC3036
127300980220      *  Scrivo record dati firmatario
127400960705     C                   MOVEL     EDSD10        WDAT
127500960729     C                   Z-ADD     §MSNUM        STAPRG
127600960705     C                   MOVEL     'SD10'        STATPR
127700960705     C                   MOVEL     WDAT          STADAT
127800960528     C                   WRITE     IFTSTA00
127900080422     c                   eval      Righe_Dett = 'S'
128000960528     C                   END
128100980220      *
128200960528     C                   ENDSR
128300980220      *-------------------------------------------------------------*
128400980220      *? WRSD15 - Scrittura record partner - dettaglio
128500980220      *----------------------------------------------------------------
128600960528     C     WRSD15        BEGSR
128700980220      *
128800980220      *  Pulisco dati DS
128900960712     C                   MOVEL     *BLANKS       WDAT
129000960528     C                   CLEAR                   EDSD15
129100980220      *
129200980220      *  Controllo se ci sono delle note di consegna
129300980220     C     VACDCM        IFNE      0                                            -- 01 --
129400980204     C                   MOVEL     *BLANKS       RDEVAL
129500980220      *
129600980220     C     WTRRDE        IFEQ      'S'                                          -- 02 --
129700980204     C                   Z-ADD     VACAAS        KAAS
129800980204     C                   Z-ADD     VACLNP        KLNP
129900980204     C                   Z-ADD     VACNRS        KNRS
130000980204     C                   Z-ADD     VACNSP        KNSP
130100980204     C                   MOVEL     'DD4440'      KNMC
130200980204     C                   Z-ADD     1             KNRP1
130300980204     C     KRDE          CHAIN     EDRDE01L                           31
130400980220      *
130500980220     C     *IN31         IFEQ      '0'                                          -- 03 --
130600980204     C                   MOVEL     'AAI'         DD4451
130700980204     C                   MOVEL     RDEVAL        DD4440
130800980204     C                   DELETE    EDRDE000
130900980220      * controllo se c'è proseguimento
131000980204     C                   Z-ADD     2             KNRP1
131100980204     C     KRDE          CHAIN     EDRDE01L                           31
131200980220      *
131300980220     C     *IN31         IFEQ      '0'                                          -- 04 --
131400980204     C                   MOVE      'AAI'         DD4451
131500980204     C                   MOVE      RDEVAL        DD4440
131600980204     C                   DELETE    EDRDE000
131700980220     C                   END                                                    -- 04 --
131800980220      *
131900980220      *  Scrivo record dati note
132000980204     C                   MOVEL     EDSD15        WDAT
132100980204     C                   Z-ADD     §MSNUM        STAPRG
132200980204     C                   MOVEL     'SD15'        STATPR
132300980204     C                   MOVEL     WDAT          STADAT
132400980204     C                   WRITE     IFTSTA00
132500080422     c                   eval      Righe_Dett = 'S'
132600980220     C                   END                                                    -- 03 --
132700980220     C                   END                                                    -- 02 --
132800980220      *-------------------------------
132900980220     C                   ELSE                                                   -- 01 --
133000980220      *-------------------------------
133100980220      *
133200980220      *  Se  è una giacenza cerco le note giacenza
133300961204     C                   Z-ADD     GCPAGC        KAGC
133400961204     C                   Z-ADD     GCPFGC        KFGC
133500961204     C                   Z-ADD     GCPNGC        KNGC
133600961204     C                   Z-ADD     10            KFAS
133700050309     C     KGNP          CHAIN     tignp01L                           31
133800980220     C     *IN31         IFEQ      '0'                                          -- 02 --
133900961204     C                   MOVEL     'AAI'         DD4451
134000961204     C                   MOVEL     GNPDMC        DD4440
134100961204     C*  Scivo record dati note giacenza
134200961204     C                   MOVEL     EDSD15        WDAT
134300961204     C                   Z-ADD     §MSNUM        STAPRG
134400961204     C                   MOVEL     'SD15'        STATPR
134500961204     C                   MOVEL     WDAT          STADAT
134600961204     C                   WRITE     IFTSTA00
134700080422     c                   eval      Righe_Dett = 'S'
134800980220     C                   END                                                    -- 02 --
134900980220     C                   END                                                    -- 01 --
135000961204     C*
135100960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
135200980220     C     WSD15         IFEQ      'S'                                          -- 01 --
135300960821     C                   MOVEL     *BLANKS       WDAT
135400960821     C                   CLEAR                   EDSD15
135500970318     C***                  MOVEL'AAI'     DD4451
135600970318     C***                  MOVELFTX,1     DD4440
135700970318     C***                  MOVELEDSD15    WDAT
135800970318     C***                  Z-ADD§MSNUM    STAPRG
135900970318     C***                  MOVEL'SD15'    STATPR
136000970318     C***                  MOVELWDAT      STADAT
136100970318     C***                  WRITEIFTSTA00
136200970318     C***                  CLEARWSD15
136300980220     C                   END                                                    -- 01 --
136400980220      *
136500960528     C                   ENDSR
136600960528     C*-------------------------------------------------------------*
136700960528     C*? WRSD20 - Scrittura record partner - dettaglio
136800980219      *----------------------------------------------------------------
136900960528     C     WRSD20        BEGSR
137000980219      *
137100980219      *  Pulisco dati DS
137200960912     C                   MOVEA     *BLANKS       SG1
137300960912     C                   MOVEA     *BLANKS       SG2
137400970318     C                   MOVEA     *BLANKS       SG3
137500970619     C                   MOVEA     *BLANKS       SG4
137600960912     C                   MOVEL     *BLANKS       WDAT
137700960528     C                   CLEAR                   EDSD20
137800960528     C                   MOVEL     '99999'       DE1496
137900960729     C                   MOVE      VACNCL        DE7224
138000980220      *
138100990604      *  Se è stata aperta C.A. elaboro a parte
138200990604     C     WELACA        IFEQ      'S'
138300990604     C                   EXSR      COLCA
138400990604     C                   GOTO      ED20
138500990604     C                   ENDIF
138600990604      *
138700990604      *  Se sto effettuando la consegna totale di una bolla con codice
138800980219      *  consegna anomala = '5' su cui è stata effettuata una consegna
138900980219      *  parziale risfleggo eventuali colli trasmessi e chiusi con IDD
139000980220      *
139100980220     C     BLPCCA        IFEQ      '5'
139200970319     C     WTREVE        OREQ      'S'
139300980220      *
139400970319     C     BLPDCM        IFGT      0
139500970318     C     BLPDCP        ANDGT     0
139600970318     C                   EXSR      LIBIDD
139700970318     C                   END
139800980220      *
139900970319     C                   END
140000980220      *
140100980220      *  Se tabellato devo sempre inviare il dettaglio segnacolli
140200980220      *  Carico tutto
140300970304     C     §PTDET        IFEQ      'S'                                          --- 02 ---
140400960912     C                   EXSR      CARCOL
140500980220     C                   EXSR      WRTCOL
140600960912     C                   END
140700980220      *
140800980223      *  Se tabellato devo inviare dettaglio solo per consegne parziali
140900980223      *  Carico non consegnati
141000970304     C     §PTDET        IFEQ      'N'                                          --- 02 ---
141100970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
141200980223      *
141300980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
141400980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
141500970303     C     BLPCCA        OREQ      '5'
141600970617     C     BLPCCA        OREQ      '3'
141700970619     C     VACCCA        OREQ      '7'
141800970619     C     VACCCA        OREQ      'P'
141900970319     C     WTREVE        OREQ      'S'
142000960912     C                   EXSR      CARCOL
142100980220     C                   EXSR      WRTCOL
142200960912     C                   ELSE
142300980223      *
142400980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
142500970304     C     BLPDCM        IFNE      0
142600970304     C                   EXSR      TUTTI
142700970304     C                   END
142800960912     C                   EXSR      WRT20
142900970304     C                   END                                                    --- 03 ---
143000970304     C                   END                                                    --- 02 ---
143100980223      *
143200980223      *  Se previsto da tabella che non devo mai passare il
143300980223      *  numero dei colli scrivo D20
143400970304     C     §PTDET        IFEQ      'M'                                          --- 02 ---
143500980223      *  Per consegne parziali o IDD loop su dettaglio segnacolli
143600970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
143700980223      *
143800980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
143900980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
144000970304     C     BLPCCA        OREQ      '5'
144100970617     C     BLPCCA        OREQ      '3'
144200970619     C     VACCCA        OREQ      '7'
144300970619     C     VACCCA        OREQ      'P'
144400970319     C     WTREVE        OREQ      'S'
144500970304     C                   EXSR      REDBLT
144600970304     C                   ELSE
144700980223      *
144800980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
144900970304     C     BLPDCM        IFEQ      0
145000970304     C                   EXSR      TUTTI
145100970304     C                   END
145200970304     C                   EXSR      WRT20
145300970304     C                   END                                                    --- 03 ---
145400970304     C                   END                                                    --- 02 ---
145500980223      *
145600990604     C     ED20          ENDSR
145700980220      *----------------------------------------------------------------
145800980220      *? LIBIDD - Controllo se devo sfleggare colli con IDD
145900980220      *----------------------------------------------------------------
146000970318     C     LIBIDD        BEGSR
146100980219      *
146200980219      *  Lettura BLT e caricamento colli
146300970318     C     KBLT          CHAIN     FNBLT01L                           31
146400980219      *  rislego i colli da trasmettete al cliente
146500980220     C     *IN31         DOWEQ     '0'
146600980220      *
146700970318     C     BLTFTR        IFEQ      'C'
146800970318     C     BLTDCM        ANDGT     0
146900970318     C     BLTFL1        ANDEQ     '5'
147000970318     C                   MOVEL     ' '           BLTFTR
147100970318     C                   EXCEPT    AGGTRA
147200970318     C                   END
147300980219      * Lettura successiva
147400970318     C     KBLT          READE     FNBLT01L                               31
147500970318     C                   END
147600980219      *
147700970318     C                   ENDSR
147800980219      *----------------------------------------------------------------
147900980219      *? CARCOL - Routine caricamento colli da dettag.
148000980219      *----------------------------------------------------------------
148100960912     C     CARCOL        BEGSR
148200980219      *
148300050413      *  ---------------------
148400050413      *  Legenda dei contatori:   Y1 = Colli Consegnati
148500050413      *  ----------------------   Y2 = Colli Non Consegnati
148600050413      *                           Y3 = Colli Chiusi con IDD
148700050413      *                           Y4 = Colli Negativi con IDD
148800050413      *
148900960912     C                   Z-ADD     0             Y1                5 0
149000960912     C                   Z-ADD     0             Y2                5 0
149100970318     C                   Z-ADD     0             Y3                5 0
149200970619     C                   Z-ADD     0             Y4                5 0
149300980219      *
149400980220      *  Lettura BLT e caricamento colli nelle schiere
149500960912     C     KBLT          CHAIN     FNBLT01L                           31
149600980223      *
149700980220     C     *IN31         DOWEQ     '0'                                          -- 01 --
149800980219      *
149900980219      *  Considero solo i colli non trasmessi al cliente
150000980220     C     BLTFTR        IFNE      'C'                                          -- 02 --
150100980220      *
150200980220      *-------------------------------------------------
150300980219      *  Controllo se devo caricare colli consegnati
150400960912     C     BLTDCM        IFGT      0
150500970303     C     BLTFL1        ANDEQ     ' '
150600970303     C     §PTDET        ANDEQ     'S'
150700970617     C     BLPCCA        ANDNE     '3'
150800980219      *
150900980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
151000980219     C     §PUBS1        IFEQ      'E'
151100980219     C                   EXSR      CARSGE                                       EUROEXPRESS
151200980219     C                   ELSE
151300980219     C                   EXSR      CARSGN                                       BARTOLINI
151400980219     C                   END
151500980219      *
151600980219     C     WSGN          IFNE      *BLANKS
151700960912     C                   ADD       1             Y1
151800960912     C                   MOVEL     WSGN          SG1(Y1)
151900960912     C                   END
152000980219     C                   END
152100980219      *
152200980220      *-------------------------------------------------
152300980219      *  Controllo se devo caricare colli non consegnati
152400960912     C     BLTDCM        IFEQ      0
152500970304     C     §PTDET        ANDNE     'M'
152600970619     C     VACCCA        ANDNE     '7'
152700970304     C     BLTFL1        OREQ      '5'
152800960913     C     §PTDET        ANDNE     'M'
152900970617     C     BLPDCM        ANDGT     0
153000970617     C     BLTFL1        OREQ      '7'
153100970617     C     §PTDET        ANDNE     'M'
153200970617     C     BLTDCM        ANDGT     0
153300970617     C     VACCCA        ANDNE     'P'
153400970617     C     BLPCCA        OREQ      '3'
153500970617     C     §PTDET        ANDNE     'M'
153600970617     C     BLPDCM        ANDGT     0
153700980219      *
153800980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
153900980219     C     §PUBS1        IFEQ      'E'
154000980219     C                   EXSR      CARSGE                                       EUROEXPRESS
154100980219     C                   ELSE
154200980219     C                   EXSR      CARSGN                                       BARTOLINI
154300980219     C                   END
154400980219      *
154500980219     C     WSGN          IFNE      *BLANKS
154600960912     C                   ADD       1             Y2
154700960912     C                   MOVEL     WSGN          SG2(Y2)
154800960912     C                   END
154900980219     C                   END
155000980220      *
155100980220      *-------------------------------------------------
155200980220      *  Controllo se devo caricare colli chiusi con IDD x evento PID
155300970318     C     BLTFL1        IFEQ      '5'
155400970318     C     §PTDET        ANDNE     'M'
155500970318     C     BLPDCM        ANDEQ     0
155600970617     C     BLTFL1        OREQ      '7'
155700970617     C     §PTDET        ANDNE     'M'
155800970617     C     VACCCA        ANDEQ     'P'
155900980220      *
156000980220      *  Se ha segnacollato il cliente carico in schiera i colli cons.
156100980219     C     §PUBS1        IFEQ      'E'
156200980219     C                   EXSR      CARSGE                                       EUROEXPRESS
156300980219     C                   ELSE
156400980219     C                   EXSR      CARSGN                                       BARTOLINI
156500980219     C                   END
156600980219      *
156700980219     C     WSGN          IFNE      *BLANKS
156800970318     C                   ADD       1             Y3
156900970318     C                   MOVEL     WSGN          SG3(Y3)
157000970318     C                   END
157100980220      *
157200980219     C                   END
157300980220      *
157400980220      *-------------------------------------------------
157500980220      *  Se devo inviare solo colli negativi ed ho IDD li carico in
157600980220      *  SG4 (nel caso non abbia ancora attivato flag FL1)
157700970619     C     §PTDET        IFEQ      'N'
157800970619     C     BLTDCM        ANDNE     0
157900970619     C     BLTFL1        ANDEQ     ' '
158000980220      *
158100970619     C     BLPCCA        IFEQ      '3'
158200970619     C     BLPCCA        OREQ      '5'
158300980220      *
158400970619     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
158500980219     C     §PUBS1        IFEQ      'E'
158600980219     C                   EXSR      CARSGE                                       EUROEXPRESS
158700980219     C                   ELSE
158800980219     C                   EXSR      CARSGN                                       BARTOLINI
158900980219     C                   END
159000980219      *
159100980219     C     WSGN          IFNE      *BLANKS
159200970619     C                   ADD       1             Y4
159300970619     C                   MOVEL     WSGN          SG4(Y4)
159400970619     C                   END
159500980219      *
159600980219     C                   END
159700970620     C                   END
159800980220      *
159900980220     C                   END                                                    -- 02 --
160000980220      *
160100050331      * Se è sempre richiesto il dettaglio Segnacolli sul Partner/Cliente ed
160200050331      * è una spedizione di RESO:
160300050331      * (poichè lo status di RESO viene inviata 2 volte da noi con 2 date differenti:
160400050331      *   la prima volta con la data della bolla di reso generata, la seconda con la
160500050331      *    data di consegna della bolla di reso. Vedi FNVAC00T)
160600050331      * ALLORA SOLO IN QUESTO CASO non si fleggano i segnacolli come trasmessi.
160700050331      *  in maniera da poterli sempre ritrasmettere al Partner/Cliente.
160800050331      * (Caso ARVATO che ragiona esclusivamente sui segnacolli e non sul rif.Sped.)
160900050331     C     §PTDET        ifEQ      'S'
161000050331     C     VACCCA        IFEQ      '2'
161100050331     C     VACCCA        OREQ      '6'
161200050331     c                   goto      salta_UPD
161300050331     c                   end
161400050331     c                   end
161500050331      *
161600980220      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
161700970304     C     BLTDCM        IFNE      0
161800970303     C                   MOVEL     'C'           BLTFTR
161900970303     C                   EXCEPT    AGGTRA
162000970304     C                   END
162100050331      *
162200050331     c     salta_UPD     tag
162300980220      *
162400980220      * Lettura successiva
162500960912     C     KBLT          READE     FNBLT01L                               31
162600980220     C                   END
162700980220      *
162800980220     C                   ENDSR
162900990604      *----------------------------------------------------------------
163000990604      *? COLCA - Routine caricamento colli da C.A.
163100990604      *----------------------------------------------------------------
163200990604     C     COLCA         BEGSR
163300990604      *
163400990604     C                   Z-ADD     *ZEROS        Y4                5 0
163500990604     C                   Z-ADD     *ZEROS        WCOLLI            5 0
163600040715      *
163700040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
163800040715      * SIA COME MAMMA CHE COME FIGLIA
163900040715     c                   clear                   fidn12ds
164000040715     c                   eval      i12aas = kAAS
164100040715     c                   eval      i12lnp = kLNP
164200040715     c                   eval      i12nrs = kNRS
164300040715     c                   eval      i12nsp = kNSP
164400040715     c                   eval      i12fel = 'E'
164500040715     c                   eval      i12fan = 'E'
164600040715     c                   eval      i12fch = 'E'
164700040715     c                   eval      i12fpt = 'E'
164800040715     c                   call      'FIDN12R'
164900040715     c                   parm                    fidn12ds
165000040715     c                   z-add     *zeros        ii                2 0
165100040715      **
165200040715      * se trovata almeno una CA
165300040715     c                   if        o12nca > 0
165400040715     c                   dow       ii <  o12nca
165500040715      **
165600040715     c                   add       1             ii
165700040715     c                   movea     skkey(ii)     dskey
165800040715     C                   z-add     dsaac         kaac
165900040715     C                   z-add     dsfil         kfil
166000040715     C                   z-add     dsnca         knca
166100040715     c     kdct          chain     FNDCT000
166200040715     c                   If        %found(fndct01l)
166300040715     C                   MOVEL     DCTFLO        DDCT01
166400040715     C     §DCTtrpt      ifeq      *BLANKS
166500990604      *
166600040715      ** Leggo testata C.A. per reperire il n. spedizione e per flaggarla come
166700040715      ***  trasmessa
166800040715     C**** KBLP          CHAIN     FNDCT000                           31
166900040715      ****
167000040715     C**** *IN31         DOWEQ     '0'                                          -- 01 --
167100040715     C****               MOVEL     DCTFLO        DDCT01
167200040715      ***Considero solo C.A.: non annullate, non chiuse e non trasmesse
167300040715     C**** DCTATB        IFEQ      *BLANKS                                      -- 02 --
167400040715     C**** DCTDCH        ANDEQ     *ZEROS
167500040715     C**** §DCTtrpt      ANDEQ     *BLANKS
167600990607      *
167700990607      *  Flaggo C.A. come trasmessa
167800020412     C                   MOVEL     'S'           §DCTtrpt
167900990607     C                   MOVEL     DDCT01        DCTFLO
168000990607     C                   EXCEPT    AGGDCT
168100990607      *
168200990604      *  Incremento numero colli
168300990604     C                   ADD       DCTNCN        WCOLLI
168400990604      *
168500990604      *  Controllo se devo caricare i segnacolli
168600990604     C     §PTDET        IFNE      'M'                                          -- 03 --
168700990604      *
168800990604      *  Leggo segnacolli C.A.: non annullati, e non chiusi
168900990604     C     KDCD          CHAIN     FNDCD000                           32
169000990607     C     *IN32         DOWEQ     '0'                                          -- 04 --
169100990604     C     DCDATB        IFEQ      *BLANKS                                      -- 05 --
169200990604     C     DCDDCH        ANDEQ     *ZEROS
169300990604      *
169400990604     C     §PUBS1        IFEQ      'E'
169500990604     C                   EXSR      CARSGE                                       EUROEXPRESS
169600990604     C                   ELSE
169700990604     C                   EXSR      CARSGN                                       BARTOLINI
169800990604     C                   ENDIF
169900990604      *
170000990604     C     WSGN          IFNE      *BLANKS
170100990604     C                   ADD       1             Y4
170200990604     C                   MOVEL     WSGN          SG4(Y4)
170300990604     C                   ENDIF
170400990604      *
170500990604     C                   ENDIF                                                  -- 05 --
170600990607     C     KDCD          READE     FNDCD000                               32
170700990604     C                   ENDDO                                                  -- 04 --
170800040715      *
170900040715     C                   ENDIF                                                  -- 03 --
171000040715      *****
171100040715     C*****              ENDIF                                                  -- 02 --
171200040715      **Lettura successiva
171300040715     C*****KBLP          READE     FNDCT000                               31
171400040715     C*****              ENDDO                                                  -- 01 --
171500040715      *****
171600040715     C                   endif
171700040715     C                   endif
171800040715      *
171900040715     c                   enddo
172000040715     c                   endif
172100040715      *
172200990604      *
172300990607      * SCRIVO FLAT FILE
172400990607     C     §PTDET        IFEQ      'M'                                          -- 01 --
172500990607     C     Y4            OREQ      *ZEROS
172600990607     C                   Z-ADD     WCOLLI        DE7224
172700990607     C                   EXSR      WRT20
172800990607     C                   ELSE
172900990607      *
173000990607     C     Y4            IFNE      *ZEROS                                       -- 02 --
173100050427      *  Nel GID imposta il numero dei Colli Danneggiati
173200050427     C                   Z-ADD     y4            DE7224
173300990607     C                   Z-ADD     *ZEROS        WW
173400990607      *
173500990607     C                   DO        Y4            WE                             -- 03 --
173600990607      *  Scrivo ogni §PUEL2 elementi
173700990607     C     WW            IFEQ      §PUEL2                                       -- 04 --
173800990607     C                   MOVEL     '24'          DE4233
173900990607     C                   EXSR      WRT20
174000990607     C                   MOVEL     *BLANKS       EDSD20
174100990607     C                   Z-ADD     *ZEROS        WW
174200990607     C                   ENDIF                                                  -- 04 --
174300990607     C                   ADD       1             WW
174400990607     C                   MOVEL     SG4(WE)       D20(WW)
174500990607     C                   ENDDO                                                  -- 03 --
174600990607      *
174700990607      *  Se ci sono ancora dei colli scrittura nuovo record
174800990607     C     WW            IFNE      *ZEROS                                       -- 03 --
174900990607     C                   MOVEL     '24'          DE4233
175000990607     C                   EXSR      WRT20
175100990607     C                   MOVEL     *BLANKS       EDSD20
175200990607     C                   ENDIF                                                  -- 03 --
175300990607      *
175400990607     C                   ENDIF                                                  -- 02 --
175500990607     C                   ENDIF                                                  -- 01 --
175600990607      *
175700990604     C                   ENDSR
175800980220      *----------------------------------------------------------------
175900980220      *? WRTCOL - Routine scrittura colli
176000980220      *----------------------------------------------------------------
176100980220     C     WRTCOL        BEGSR
176200980220      *
176300980220      *  Scrivo i colli non consegnati
176400980220     C     Y2            IFGT      0
176500960912     C                   MOVE      Y2            DE7224
176600960912     C                   Z-ADD     0             WW                3 0
176700980220      *
176800980220     C                   DO        Y2            WE                3 0
176900980220      *
177000990607      *  Scrivo ogni §PUEL2 elementi
177100990607     C     WW            IFEQ      §PUEL2
177200990607     C                   MOVEL     '24'          DE4233
177300990607     C                   EXSR      WRT20
177400990607     C                   MOVEL     *BLANKS       EDSD20
177500990607     C                   Z-ADD     0             WW
177600990607     C                   ENDIF
177700990607     C                   ADD       1             WW
177800990607     C                   MOVEL     SG2(WE)       D20(WW)
177900990607     C                   ENDDO
178000990607      *
178100990607      *  Se ci sono ancora dei colli scrittura nuovo record
178200990607     C     WW            IFNE      *ZEROS
178300990607     C                   MOVEL     '24'          DE4233
178400990607     C                   EXSR      WRT20
178500990607     C                   MOVEL     *BLANKS       EDSD20
178600990607     C                   ENDIF
178700990607      *
178800990607     C                   ENDIF
178900980220      *
179000980220      *  Scrivo i colli consegnati
179100980220     C     Y1            IFGT      0                                            -- 01 --
179200980223      *
179300980220      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
179400980220      *  (conse. parziale --> ci sono colli non consegnati)
179500980220      *  (pratica IDD --> ci sono colli persi e colli consegnati)
179600980220     C     Y2            IFGT      0                                            -- 02 --
179700081216     C                   ADD       1             WPROG
179800960912     C                   MOVEL     WPROG         DA1490
179900960912     C                   MOVEL     '9999'        DA1312
180000960912     C                   CLEAR                   WDAT
180100960912     C                   MOVEL     EDSD00        WDAT
180200960912     C                   Z-ADD     §MSNUM        STAPRG
180300960912     C                   MOVEL     'SD00'        STATPR
180400960912     C                   MOVEL     WDAT          STADAT
180500960912     C                   WRITE     IFTSTA00
180600080422     c                   eval      Righe_Dett = 'S'
180700980223      *
180800980220      *  Consegna !!!!!!!
180900960912     C                   Z-ADD     1             X
181000980220     C     'CPA'         LOOKUP    C2A(X)                                 32       ?? CONSE.TOT
181100980220      *
181200980220     C     *IN32         IFEQ      '1'                                          -- 03 --
181300980220     C     §PULAB        IFEQ      'S'                                          -- 04 --
181400961022     C                   MOVEL     CEV(X)        DB9013
181500961022     C                   MOVEL     STS(X)        DB9011
181600980220     C                   ELSE                                                   -- 04 --
181700961022     C                   MOVEL     CDV(X)        DB9013
181800961022     C                   MOVEL     STV(X)        DB9011
181900980220     C                   END                                                    -- 04 --
182000960912     C                   MOVEL     '7  '         DB2005
182100960913     C                   MOVEL     BLPDCP        WDATHM
182200960912     C                   MOVE      VACHMC        WDATHM           12 0
182300960912     C                   MOVEL     WDATHM        DB2380
182400960912     C                   MOVEL     '203'         DB2379
182500980220     C                   END                                                    -- 03 --
182600980223      *
182700980223      *  Scrivo record evento CONSEGNA
182800960912     C                   MOVEL     EDSD05        WDAT
182900960912     C                   Z-ADD     §MSNUM        STAPRG
183000960912     C                   MOVEL     'SD05'        STATPR
183100960912     C                   MOVEL     WDAT          STADAT
183200960912     C                   WRITE     IFTSTA00
183300080422     c                   eval      Righe_Dett = 'S'
183400980223      *
183500980223      *  Pulisco SD20
183600960913     C                   CLEAR                   EDSD20
183700960913     C                   MOVEL     '99999'       DE1496
183800980220     C                   END                                                    -- 02 --
183900980223      *
184000980223      *  Scrivo D20 da SG1
184100960912     C                   MOVE      Y1            DE7224
184200960912     C                   Z-ADD     0             WW                3 0
184300980220     C                   DO        Y1            WE                3 0          -- 02 --
184400980223      *
184500990607      *  Scrivo ogni §PUEL2 elementi
184600990607     C     WW            IFEQ      §PUEL2
184700960913     C                   MOVEL     '24'          DE4233
184800960912     C                   EXSR      WRT20
184900990607     C                   MOVEL     *BLANKS       EDSD20
185000990607     C                   Z-ADD     *ZEROS        WW
185100990607     C                   ENDIF
185200990607     C                   ADD       1             WW
185300990607     C                   MOVEL     SG1(WE)       D20(WW)
185400990607     C                   ENDDO                                                  -- 02 --
185500980223      *
185600990607      *  Se ci sono ancora dei colli scrittura nuovo record
185700990607     C     WW            IFNE      *ZEROS                                       -- 02 --
185800990607     C                   MOVEL     '24'          DE4233
185900990607     C                   EXSR      WRT20
186000960912     C                   MOVEL     *BLANKS       EDSD20
186100990607     C                   ENDIF                                                  -- 02 --
186200980223      *
186300980220     C                   ELSE                                                   -- 01 --
186400980223      *
186500980223      *  Scrivo D20 da SG1
186600970619     C                   Z-ADD     Y1            DE7224
186700970619     C                   Z-ADD     0             WW                3 0
186800980220     C                   DO        Y1            WE                3 0          -- 02 --
186900990607      *
187000990607      *  Scrivo ogni §PUEL2 elementi
187100990607     C     WW            IFEQ      §PUEL2
187200990607     C                   MOVEL     '24'          DE4233
187300990607     C                   EXSR      WRT20
187400990607     C                   MOVEL     *BLANKS       EDSD20
187500990607     C                   Z-ADD     *ZEROS        WW
187600990607     C                   ENDIF
187700990607     C                   ADD       1             WW
187800990607     C                   MOVEL     SG1(WE)       D20(WW)
187900990607     C                   ENDDO                                                  -- 02 --
188000990607      *
188100990607      *  Se ci sono ancora dei colli scrittura nuovo record
188200990607     C     WW            IFNE      *ZEROS                                       -- 02 --
188300990607     C                   MOVEL     '24'          DE4233
188400990607     C                   EXSR      WRT20
188500990607     C                   MOVEL     *BLANKS       EDSD20
188600990607     C                   ENDIF                                                  -- 02 --
188700980223      *
188800980220     C                   END                                                    -- 01 --
188900980223      *
189000980223      *  Controllo se devo scrivere colli chiusi x IDD parziale
189100980220     C     Y3            IFGT      0                                            -- 01 --
189200980223      *
189300980223      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
189400980223      *  (conse.parziale --> ci sono colli non consegnati)
189500980223      *  (pratica IDD --> ci sono colli persi e colli consegnati)
189600980220     C     Y2            IFGT      0                                            -- 02 --
189700081216     C                   ADD       1             WPROG
189800970318     C                   MOVEL     WPROG         DA1490
189900970318     C                   MOVEL     '9999'        DA1312
190000970318     C                   CLEAR                   WDAT
190100970318     C                   MOVEL     EDSD00        WDAT
190200970318     C                   Z-ADD     §MSNUM        STAPRG
190300970318     C                   MOVEL     'SD00'        STATPR
190400970318     C                   MOVEL     WDAT          STADAT
190500970318     C                   WRITE     IFTSTA00
190600080422     c                   eval      Righe_Dett = 'S'
190700980223      *
190800980223      *  Consegna !!!!!!!
190900970318     C                   Z-ADD     1             X
191000970619     C     VACCCA        IFEQ      '7'
191100970617     C     'MAN'         LOOKUP    C2A(X)                                 32
191200990607     C                   ELSE
191300970617     C     'PID'         LOOKUP    C2A(X)                                 32
191400990607     C                   END
191500980220      *
191600990607     C     *IN32         IFEQ      '1'                                          -- 03 --
191700990607     C     §PULAB        IFEQ      'S'
191800970318     C                   MOVEL     CEV(X)        DB9013
191900970318     C                   MOVEL     STS(X)        DB9011
192000990607     C                   ELSE
192100970318     C                   MOVEL     CDV(X)        DB9013
192200970318     C                   MOVEL     STV(X)        DB9011
192300990607     C                   END
192400970318     C                   MOVEL     '7  '         DB2005
192500970318     C                   MOVEL     BLPDCP        WDATHM
192600970318     C                   MOVE      VACHMC        WDATHM           12 0
192700970318     C                   MOVEL     WDATHM        DB2380
192800970318     C                   MOVEL     '203'         DB2379
192900990607     C                   END                                                    -- 03 --
193000980223      *
193100980223      *  Scivo record evento CONSEGNA
193200970318     C                   MOVEL     EDSD05        WDAT
193300970318     C                   Z-ADD     §MSNUM        STAPRG
193400970318     C                   MOVEL     'SD05'        STATPR
193500970318     C                   MOVEL     WDAT          STADAT
193600970318     C                   WRITE     IFTSTA00
193700080422     c                   eval      Righe_Dett = 'S'
193800980223      *
193900980223      *  Pulisco SD20
194000970318     C                   CLEAR                   EDSD20
194100970318     C                   MOVEL     '99999'       DE1496
194200990607     C                   END                                                    -- 02 --
194300980223      *
194400980223      *  Scrivo D20 da SG3
194500970318     C                   MOVE      Y3            DE7224
194600970318     C                   Z-ADD     0             WW                3 0
194700990607     C                   DO        Y3            WE                3 0          -- 02 --
194800990607      *
194900990607      *  Scrivo ogni §PUEL2 elementi
195000990607     C     WW            IFEQ      §PUEL2
195100990607     C                   MOVEL     '24'          DE4233
195200990607     C                   EXSR      WRT20
195300990607     C                   MOVEL     *BLANKS       EDSD20
195400990607     C                   Z-ADD     *ZEROS        WW
195500990607     C                   ENDIF
195600990607     C                   ADD       1             WW
195700990607     C                   MOVEL     SG3(WE)       D20(WW)
195800990607     C                   ENDDO                                                  -- 02 --
195900990607      *
196000990607      *  Se ci sono ancora dei colli scrittura nuovo record
196100990607     C     WW            IFNE      *ZEROS                                       -- 02 --
196200990607     C                   MOVEL     '24'          DE4233
196300990607     C                   EXSR      WRT20
196400990607     C                   MOVEL     *BLANKS       EDSD20
196500990607     C                   ENDIF                                                  -- 02 --
196600980220      *
196700990607     C                   ENDIF                                                  -- 01 --
196800990607      *
196900980220      * Per CCA=5 o CCA=3 e  §PTDET='N' Y2, Y3 = 0 e Y4 > 0 imposto Y4 e SG4
197000990607     C     BLPCCA        IFEQ      '5'                                          -- 01 --
197100970619     C     BLPCCA        OREQ      '3'
197200980220      *
197300990607     C     §PTDET        IFEQ      'N'                                          -- 02 --
197400970619     C     Y2            ANDEQ     0
197500970619     C     Y3            ANDEQ     0
197600970619     C     Y4            ANDNE     0
197700970619     C                   Z-ADD     Y4            DE7224
197800970619     C                   Z-ADD     0             WW                3 0
197900990607     C                   DO        Y4            WE                3 0          -- 03 --
198000990607      *
198100990607      *  Scrivo ogni §PUEL2 elementi
198200990607     C     WW            IFEQ      §PUEL2
198300990607     C                   MOVEL     '24'          DE4233
198400990607     C                   EXSR      WRT20
198500990607     C                   MOVEL     *BLANKS       EDSD20
198600990607     C                   Z-ADD     *ZEROS        WW
198700990607     C                   ENDIF
198800990607     C                   ADD       1             WW
198900990607     C                   MOVEL     SG4(WE)       D20(WW)
199000990607     C                   ENDDO                                                  -- 03 --
199100990607      *
199200990607      *  Se ci sono ancora dei colli scrittura nuovo record
199300990607     C     WW            IFNE      *ZEROS                                       -- 03 --
199400990607     C                   MOVEL     '24'          DE4233
199500990607     C                   EXSR      WRT20
199600990607     C                   MOVEL     *BLANKS       EDSD20
199700990607     C                   ENDIF                                                  -- 03 --
199800990607      *
199900990607     C                   END                                                    -- 02 --
200000990607     C                   END                                                    -- 01 --
200100980223      *
200200960912     C                   ENDSR
200300980223      *----------------------------------------------------------------
200400980223      *? TUTTI - fleggo tutti i record di BLT come trasmessi
200500980223      *----------------------------------------------------------------
200600970304     C     TUTTI         BEGSR
200700980223      *
200800980223      *  Aggiorno come trasmessi i singoli colli
200900970303     C     KBLT          CHAIN     FNBLT01L                           31
201000970303     C     *IN31         DOWEQ     '0'
201100980223      *
201200980223      *  Considero solo i colli non trasmessi al cliente
201300970303     C     BLTFTR        IFNE      'C'
201400980223      *
201500980223      *  Fleggo segnacolli come trasmessi se consegnati
201600970303     C     BLTDCM        IFGT      0
201700980223      *
201800980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
201900970303     C                   MOVEL     'C'           BLTFTR
202000970303     C                   EXCEPT    AGGTRA
202100970303     C                   END
202200970304     C                   END
202300980223      *
202400980223      * Lettura successiva
202500970303     C     KBLT          READE     FNBLT01L                               31
202600970303     C                   END
202700980223      *
202800970304     C                   ENDSR
202900980223      *----------------------------------------------------------------
203000980223      *? REDBLT - fleggo tutti i record di BLT come trasmessi
203100980223      *----------------------------------------------------------------
203200970304     C     REDBLT        BEGSR
203300980223      *
203400970304     C                   Z-ADD     0             WCOLCO            5 0
203500970304     C                   Z-ADD     0             WCOLID            5 0
203600980223      *
203700980223      *  Aggiorno come trasmessi i singoli colli
203800970304     C     KBLT          CHAIN     FNBLT01L                           31
203900970304     C     *IN31         DOWEQ     '0'
204000980223      *
204100980223      *  Considero solo i colli non trasmessi al cliente
204200970304     C     BLTFTR        IFNE      'C'
204300980223      *
204400980223      *  Fleggo segnacolli come trasmessi se consegnati
204500970304     C     BLTDCM        IFGT      0
204600970320     C     BLTFL1        OREQ      '5'
204700970617     C     BLTFL1        OREQ      '7'
204800970617     C     BLPCCA        OREQ      '3'
204900980223      *
205000980223      *  Se chiusi con pratica IDD aggiungo 1 al relativo nr.colli
205100980223      *  altrimenti CONTROLLO SE sono colli consegnati
205200970304     C     BLTFL1        IFEQ      '5'
205300970617     C     BLTFL1        OREQ      '7'
205400970617     C     BLPCCA        OREQ      '3'
205500970304     C                   ADD       1             WCOLID
205600970304     C                   ELSE
205700970304     C                   ADD       1             WCOLCO
205800970320     C                   END
205900980223      *
206000980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
206100970304     C                   MOVEL     'C'           BLTFTR
206200970304     C                   EXCEPT    AGGTRA
206300970304     C                   END
206400970304     C                   END
206500980223      *
206600980223      * Lettura successiva
206700970304     C     KBLT          READE     FNBLT01L                               31
206800970304     C                   END
206900980223      *
207000980223      * Se ci sono dei colli chiusi con pratica IDD scrivo TD20 per loro
207100970304     C     WCOLID        IFNE      0
207200970304     C                   MOVE      WCOLID        DE7224
207300970304     C                   EXSR      WRT20
207400980223      *
207500980223      * Se ci sono anche dei colli consegnati normalmente riscrivo SD00 e SD05
207600970304     C     WCOLCO        IFNE      0
207700081216     C                   ADD       1             WPROG
207800970304     C                   MOVEL     WPROG         DA1490
207900970304     C                   MOVEL     '9999'        DA1312
208000970304     C                   CLEAR                   WDAT
208100970304     C                   MOVEL     EDSD00        WDAT
208200970304     C                   Z-ADD     §MSNUM        STAPRG
208300970304     C                   MOVEL     'SD00'        STATPR
208400970304     C                   MOVEL     WDAT          STADAT
208500970304     C                   WRITE     IFTSTA00
208600080422     c                   eval      Righe_Dett = 'S'
208700980223      *
208800980223      * consegna parziale
208900970304     C                   Z-ADD     1             X
209000970304     C     'CPA'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
209100970304     C     *IN32         IFEQ      '1'
209200970304     C     §PULAB        IFEQ      'S'
209300970304     C                   MOVEL     CEV(X)        DB9013
209400970304     C                   MOVEL     STS(X)        DB9011
209500970304     C                   ELSE
209600970304     C                   MOVEL     CDV(X)        DB9013
209700970304     C                   MOVEL     STV(X)        DB9011
209800970304     C                   END
209900970304     C                   MOVEL     '7  '         DB2005
210000970304     C                   MOVEL     BLPDCP        WDATHM
210100970304     C                   MOVE      VACHMC        WDATHM           12 0
210200970304     C                   MOVEL     WDATHM        DB2380
210300970304     C                   MOVEL     '203'         DB2379
210400970304     C                   END
210500980223      *
210600980223      *  Scivo record evento CONSEGNA
210700970304     C                   MOVEL     EDSD05        WDAT
210800970304     C                   Z-ADD     §MSNUM        STAPRG
210900970304     C                   MOVEL     'SD05'        STATPR
211000970304     C                   MOVEL     WDAT          STADAT
211100970304     C                   WRITE     IFTSTA00
211200080422     c                   eval      Righe_Dett = 'S'
211300980223      *
211400980223      *  Pulisco SD20
211500970304     C                   CLEAR                   EDSD20
211600970304     C                   MOVEL     '99999'       DE1496
211700970304     C                   END
211800970304     C                   END
211900980223      *
212000980223      * Scrivo colli consegnati regolarmente
212100970304     C     WCOLCO        IFNE      0
212200970304     C                   MOVE      WCOLCO        DE7224
212300970304     C                   EXSR      WRT20
212400970304     C                   END
212500980223      *
212600970320     C                   ENDSR
212700980220      *----------------------------------------------------------------
212800980220      *? CHKBLT - Controllo se ci sono colli da trasmettere
212900980220      *----------------------------------------------------------------
213000970320     C     CHKBLT        BEGSR
213100980220      *
213200980220      *  Aggiorno come trasmessi i singoli colli
213300970320     C                   MOVEL     'N'           WTRCOL            1
213400970320     C     KBLT          CHAIN     FNBLT01L                           31
213500980220      *
213600970320     C     *IN31         DOWEQ     '0'
213700970320     C     WTRCOL        ANDEQ     'N'
213800980220      *  Considero solo i colli non trasmessi al cliente
213900970320     C     BLTFTR        IFNE      'C'
214000980220      *  Fleggo segnacolli come trasmessi se consegnati
214100970320     C     BLTDCM        IFGT      0
214200970320     C     BLTFL1        OREQ      '5'
214300970320     C                   MOVEL     'S'           WTRCOL
214400970320     C                   END
214500970320     C                   END
214600980220      * Lettura successiva
214700970320     C     KBLT          READE     FNBLT01L                               31
214800970320     C                   END
214900980220      *
215000970320     C                   ENDSR
215100980223      *----------------------------------------------------------------
215200980223      *? WRT20 - scrivo record d20
215300980223      *----------------------------------------------------------------
215400970304     C     WRT20         BEGSR
215500980223      *  Scrivo record SD20
215600960912     C                   MOVEL     EDSD20        WDAT
215700960912     C                   MOVEL     'SD20'        STATPR
215800960912     C                   Z-ADD     §MSNUM        STAPRG
215900960912     C                   MOVEL     WDAT          STADAT
216000960912     C                   WRITE     IFTSTA00
216100080422     c                   eval      Righe_Dett = 'S'
216200980223      *
216300960912     C                   ENDSR
216400980223      *----------------------------------------------------------------
216500980223      *? CARSGE - caricamento segnacollo EUROEXPRESS
216600980223      *----------------------------------------------------------------
216700980219     C     CARSGE        BEGSR
216800980219      *
216900990607     C     VACCCA        IFEQ      'A'
217000990607     C                   Z-ADD     DCDFLS        KFLS
217100990607     C                   Z-ADD     DCDLNA        KLNA
217200990607     C                   Z-ADD     DCDNRS        KNRS
217300990607     C                   Z-ADD     DCDNSC        KNSC
217400990607     C                   ELSE
217500990607     C                   Z-ADD     BLTFLS        KFLS
217600990607     C                   Z-ADD     BLTLNA        KLNA
217700990607     C                   Z-ADD     BLTNRS        KNRS
217800990607     C                   Z-ADD     BLTNSC        KNSC
217900990607     C                   ENDIF
218000050120      *
218100050120     C                   MOVEL     'E'           KTRC
218200050120      *
218300050120      * Controlla se non è Euroexpress ma un cliente normale come Disk C
218400050120     C     kfls          chain     AZORG01L
218500050120     C                   clear                   OG143
218600050120     c                   if        %Found(AZORG01L)
218700050120     C                   movel     ORGDE3        OG143
218800050120     C     §OGntw        IFne      'EEX'
218900050120     C                   z-add     1             KKUT
219000050120     C                   movel     '1B'          KCOD
219100050120     C                   movel(p)  §ptCTM        KKEY
219200050120     C     KTAB2         chain     tabel00f
219300050120     c                   if        %Found(tabel00f)
219400050120     c                   movel     tbluni        ds1B
219500050120      * se si tratta di un Disk "C" non Euroexpress --> Cliente
219600050120     c                   if        §1BF12 <> 'N'
219700050120     C                   MOVEL     'C'           KTRC
219800050120     c                   end
219900050120     c                   end
220000050120     c                   end
220100050120     c                   end
220200050120      *
220300980219     C                   MOVEL     *BLANKS       WSGN
220400980219      *
220500051017     C     KARS          CHAIN     FIARS000                           33
220600051017     C  N33              MOVEL     ARSNOT        WSGN
220700980219      *
220800980219     C                   ENDSR
220900980223      *----------------------------------------------------------------
221000980223      *? CARSGN - caricamento segnacollo
221100980223      *----------------------------------------------------------------
221200980219     C     CARSGN        BEGSR
221300980219      *
221400980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
221500960912     C                   MOVEA     *BLANKS       C35
221600960912     C                   MOVEL     *BLANKS       WSGN             35
221700980219      *  Compongo lnp se la devo passare
221800960912     C     §PULP2        IFNE      0
221900960912     C                   Z-ADD     §PULP2        WA                3 0
222000961029     C                   MOVEL     BLPFLS        WLNP              3
222100960912     C                   MOVEA     WLNP          C35(WA)
222200960912     C                   END
222300980219      *  Compongo lna se la devo passare
222400960912     C     §PULA2        IFNE      0
222500990607     C                   Z-ADD     §PULA2        WA
222600960912     C                   MOVEL     BLPLNA        WLNA              3
222700960912     C                   MOVEA     WLNA          C35(WA)
222800960912     C                   END
222900980219      *  Compongo nrs se lo devo passare
223000960912     C     §PUNR2        IFNE      0
223100990607     C                   Z-ADD     §PUNR2        WA
223200960912     C                   MOVEL     BLPNRS        WNRS              2
223300960912     C                   MOVEA     WNRS          C35(WA)
223400960912     C                   END
223500980219      *  Compongo nsc se la devo passare
223600960912     C     §PUSG2        IFNE      0
223700990607     C                   Z-ADD     §PUSG2        WA
223800990607     C     VACCCA        IFEQ      'A'
223900990604     C                   MOVEL     DCDNSC        WNSC              7
224000990604     C                   ELSE
224100990604     C                   MOVEL     BLTNSC        WNSC
224200990604     C                   END
224300960912     C                   MOVEA     WNSC          C35(WA)
224400960912     C                   END
224500980219      *  Compongo znc se la devo passare
224600960912     C     §PUZN2        IFNE      0
224700990607     C                   Z-ADD     §PUZN2        WA
224800990607     C                   MOVEL     BLPZNC        WZNC              2
224900960912     C                   MOVEA     WZNC          C35(WA)
225000960912     C                   END
225100980219      *  metto C35 in WSGN
225200960912     C                   MOVEA     C35           WSGN
225300980227      *
225400960730     C                   ENDSR
225500960927     C*----------------------------------------------------------------
225600960927     C*? CARNUM - CARICAMENTO NUMERATORE
225700960927     C*----------------------------------------------------------------
225800960927     C     CARNUM        BEGSR
225900960927     C*
226000960927     C                   Z-ADD     0             WPROG             4 0
226100960927     C                   Z-ADD     0             X                 3 0
226200960927     C                   Z-ADD     1             §MSNUM
226300960927     C                   MOVEL     'MS'          KCOD1
226400960927     C                   MOVEL     *BLANKS       KKEY1
226500960927     C                   MOVEL     '784'         KKEY1
226600960927     C     KTABE         CHAIN     EDTAB01L                           30
226700960927     C     *IN30         IFEQ      '0'
226800960927     C     TABFLG        ANDEQ     *BLANKS
226900020412     C                   MOVEL     TABUNI        EDIDSMS
227000960927     C                   ADD       1             §MSNUM
227100061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
227200061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
227300061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
227400061219     c                   move      §MSNUM        campo_4n          4 0
227500061219     c                   if        campo_4n = 0
227600061219     c                   add       1             §MSNUM
227700061219     c                   end
227800960927     C                   Z-ADD     WOGGI         §MSDAT
227900020412     C                   MOVEL     EDIDSMS       TABUNI
228000960927     C                   UPDATE    EDTAB000
228100960927     C                   END
228200980223      *
228300960927     C                   ENDSR
228400980223      *----------------------------------------------------------------
228500980223      *? *INZSR - OPERAZIONI INIZIALI
228600980223      *----------------------------------------------------------------
228700960528     C     *INZSR        BEGSR
228800980223      *
228900080418      *  *entry vecchia gestione con il FNVAC multimebro
229000080418     C**** *ENTRY        PLIST
229100080418     C*******            PARM                    MBR              10            da FNVAC
229200080418     C*******            PARM                    TPR               1            Europolitan
229300080418     C*******            PARM                    TerPar            3            x Filiali
229400080418      *
229500080418      *?  Deve leggere il nuovo TIVGD x generare EDI e fare il Download...  ?
229600080418     C     *ENTRY        PLIST
229700080418     C                   PARM                    CLIENTE           8            "0xxxyyyy"
229800080418     C                   PARM                    Tipo_File         2            "VC"
229900080418     C                   PARM                    Tipo_Scarico      2            "EW"
230000080418     C                   PARM                    Righe_Dett        1            "N"
230100080418     c                   eval      Righe_Dett = 'N'
230200080418      *
230300980223      * Definisco chiavi di accesso
230400080418     C     KEY_VGD01     KLIST
230500080418     C                   KFLD                    Tipo_File                      "VC"
230600080418     C                   KFLD                    CLIENTE                        "0xxxyyyy"
230700080418     C                   KFLD                    Tipo_Scarico                   "EW"
230800080418      *
230900960726     C     KTABE         KLIST
231000960726     C                   KFLD                    KCOD1
231100960726     C                   KFLD                    KKEY1
231200960528     C     KBLP          KLIST
231300960528     C                   KFLD                    KAAS
231400960528     C                   KFLD                    KLNP
231500960528     C                   KFLD                    KNRS
231600960528     C                   KFLD                    KNSP
231700990604     C     KDCD          KLIST
231800990604     C                   KFLD                    DCTAAC
231900990604     C                   KFLD                    DCTFIL
232000990604     C                   KFLD                    DCTNCA
232100960528     C     KGCP          KLIST
232200960528     C                   KFLD                    KAAS
232300960528     C                   KFLD                    KLNP
232400960528     C                   KFLD                    KNRS
232500960528     C                   KFLD                    KNSP
232600960528     C                   KFLD                    KFRG
232700050922     C     KGCp_1        KLIST
232800050922     C                   KFLD                    KAAS
232900050922     C                   KFLD                    KLNP
233000050922     C                   KFLD                    KNRS
233100050922     C                   KFLD                    KNSP
233200961204     C     KGNP          KLIST
233300961204     C                   KFLD                    KAGC
233400961204     C                   KFLD                    KFGC
233500961204     C                   KFLD                    KNGC
233600961204     C                   KFLD                    KFRG
233700961204     C                   KFLD                    KFAS
233800960528     C     KBLT          KLIST
233900960528     C                   KFLD                    KAAS
234000960528     C                   KFLD                    KLNP
234100960528     C                   KFLD                    KNRS
234200960528     C                   KFLD                    KNSP
234300970319     C     KLBL1         KLIST
234400970319     C                   KFLD                    KAAN
234500970319     C                   KFLD                    KLPN
234600970319     C                   KFLD                    KNRN
234700970319     C                   KFLD                    KNSN
234800970319     C     KLBL2         KLIST
234900970319     C                   KFLD                    KAAP
235000970319     C                   KFLD                    KLPP
235100970319     C                   KFLD                    KNRP
235200970319     C                   KFLD                    KNSP
235300060213     C     Kar4          KLIST
235400960528     C                   KFLD                    KAAS
235500960528     C                   KFLD                    KLNP
235600960528     C                   KFLD                    KNRS
235700960528     C                   KFLD                    KNSP
235800960528     C                   KFLD                    KTRC
235900960628     C     KRDE          KLIST
236000960628     C                   KFLD                    KAAS
236100960628     C                   KFLD                    KLNP
236200960628     C                   KFLD                    KNRS
236300960628     C                   KFLD                    KNSP
236400960628     C                   KFLD                    KNMC
236500970319     C                   KFLD                    KNRP1
236600961029     C     KTAB1         KLIST
236700961029     C                   KFLD                    KKUT
236800961029     C                   KFLD                    KCOD
236900970319     C     KTAB2         KLIST
237000970319     C                   KFLD                    KKUT
237100970319     C                   KFLD                    KCOD
237200970319     C                   KFLD                    KKEY
237300970319     C     KEVB          KLIST
237400970319     C                   KFLD                    KAA2
237500970319     C                   KFLD                    KLNP
237600970319     C                   KFLD                    KNRS
237700970319     C                   KFLD                    KNSP
237800970319     C                   KFLD                    KCEV
237900980204     C     KEVB1         KLIST
238000980204     C                   KFLD                    KAA2
238100980204     C                   KFLD                    KLNP
238200980204     C                   KFLD                    KNRS
238300980204     C                   KFLD                    KNSP
238400980204     C                   KFLD                    KDTV
238500980204     C                   KFLD                    KORV
238600980204     C     KEVB2         KLIST
238700980204     C                   KFLD                    KAA2
238800980204     C                   KFLD                    KLNP
238900980204     C                   KFLD                    KNRS
239000980204     C                   KFLD                    KNSP
239100051017     C     KARS          KLIST
239200980219     C                   KFLD                    KFLS
239300980219     C                   KFLD                    KLNA
239400980219     C                   KFLD                    KNRS
239500980219     C                   KFLD                    KNSC
239600980219     C                   KFLD                    KTRC
239700040607     C     KARF          klist
239800040607     C                   kfld                    KAAS
239900040607     C                   kfld                    KLNP
240000040607     C                   kfld                    KNRS
240100040607     C                   kfld                    KNSP
240200040715
240300040715     C     Kdct          klist
240400040715     C                   KFLD                    kaac
240500040715     C                   KFLD                    kfil
240600040715     C                   KFLD                    knca
240700040715      *
240800980223      * Definizione campi di work
240900980219     C     *LIKE         DEFINE    BLTNSC        KNSC
241000980219     C     *LIKE         DEFINE    BLTLNA        KLNA
241100051017     C     *LIKE         DEFINE    ARSFLS        KFLS
241200980219     C     *LIKE         DEFINE    TBLKUT        KKUT
241300961029     C     *LIKE         DEFINE    TBLCOD        KCOD
241400970319     C     *LIKE         DEFINE    TBLKEY        KKEY
241500961029     C     *LIKE         DEFINE    TABCOD        KCOD1
241600960726     C     *LIKE         DEFINE    TABKEY        KKEY1
241700960528     C     *LIKE         DEFINE    BLPAAS        KAAS
241800960528     C     *LIKE         DEFINE    BLPLNP        KLNP
241900960528     C     *LIKE         DEFINE    BLPNRS        KNRS
242000960528     C     *LIKE         DEFINE    BLPNSP        KNSP
242100060213     C     *LIKE         DEFINE    ar4TRC        KTRC
242200960528     C     *LIKE         DEFINE    GCPFRG        KFRG
242300960628     C     *LIKE         DEFINE    RDENMC        KNMC
242400970319     C     *LIKE         DEFINE    RDENRP        KNRP1
242500961204     C     *LIKE         DEFINE    GNPAGC        KAGC
242600961204     C     *LIKE         DEFINE    GNPFGC        KFGC
242700961204     C     *LIKE         DEFINE    GNPNGC        KNGC
242800961204     C     *LIKE         DEFINE    GNPFAS        KFAS
242900970319     C     *LIKE         DEFINE    LBLAAN        KAAN
243000970319     C     *LIKE         DEFINE    LBLLPN        KLPN
243100970319     C     *LIKE         DEFINE    LBLNRN        KNRN
243200970319     C     *LIKE         DEFINE    LBLNSN        KNSN
243300970319     C     *LIKE         DEFINE    LBLAAP        KAAP
243400970319     C     *LIKE         DEFINE    LBLLPP        KLPP
243500970319     C     *LIKE         DEFINE    LBLNRP        KNRP
243600970319     C     *LIKE         DEFINE    EVBAAS        KAA2
243700970319     C     *LIKE         DEFINE    EVBCEV        KCEV
243800980204     C     *LIKE         DEFINE    EVBDTV        KDTV
243900980204     C     *LIKE         DEFINE    EVBORV        KORV
244000980223      *
244100980223      * Recupero codice AS
244200960528     C                   Z-ADD     1             CODUT             1 0
244300960705     C                   CALL      'X§PARUT'
244400020412     C                   PARM                    UT§DSE0F
244500960528     C                   MOVEL     REC80         CNCR80
244600980223      *
244700980223      * Recupero data e ora x messaggio
244800960528     C                   TIME                    WHHDAT           14 0
244900960708     C                   MOVE      WHHDAT        G02DAT
245000960528     C                   MOVE      WHHDAT        DATA              6 0
245100960528     C                   MOVEL     *BLANK        G02ERR
245200960528     C                   CALL      'XSRDA8'
245300960528     C                   PARM                    WLBDA8
245400960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
245500960708     C                   MOVEL     WHHDAT        WHHMM             4 0
245600960528     C                   MOVE      WHHDAT        WANNO             2 0
245700100304     C                   Z-ADD     woggi         WOGGIymd          6 0           UDATE
245800980629      * ---------------------------------------------
245900980629      *  CARICO TABELLE
246000980629      * ---------------------------------------------
246100980223      * Caricamento Tabella eventi con IDD
246200050909      *    o tabella Giacenze/Lasciati Avviso
246300970319     C                   Z-ADD     0             X
246400050909     C                   Z-ADD     0             Xj                3 0
246500050922     C                   Z-ADD     0             Xa                3 0
246600970319     C                   Z-ADD     1             KKUT
246700970319     C                   MOVEL     '2A'          KCOD
246800970319     C                   MOVEA     *BLANKS       IDD
246900050922     C                   MOVEA     *BLANKS       GiaC
247000050926     C                   MOVEA     *BLANKS       LAvv
247100970319     C     KTAB1         CHAIN     TABEL00F                           30
247200970319     C     *IN30         DOWEQ     '0'
247300970319     C     TBLFLG        IFEQ      *BLANKS
247400970319     C                   MOVEL     TBLUNI        DS2A
247500050909      * con IDD
247600970319     C     §2AIDD        IFNE      *BLANKS
247700970319     C                   ADD       1             X
247800970319     C                   MOVEL     TBLKEY        IDD(X)
247900970319     C                   END
248000050909      *
248100050922      * Cod. Giacenza
248200050926     C                   if        §2AFTC = 'G'
248300050909     C                   add       1             Xj
248400050922     C                   MOVEL     TBLKEY        GiaC(Xj)
248500050909     C                   end
248600050922      * Lasciato Avviso
248700050926     C                   if        §2AFTC = 'A'
248800050922     C                   add       1             Xa
248900050926     C                   MOVEL     TBLKEY        LAvv(Xa)
249000050922     C                   end
249100050922      *
249200970319     C                   END
249300970319     C     KTAB1         READE     TABEL00F                               30
249400970319     C                   END
249500980629      *
249600020418      * Caricamento £1
249700030926     c***********        clear                   trul06ds
249800030926     c***********        eval      d06cod = '£1'
249900030924     c*******            movel     simfel        d06key
250000030926     c***********        movel     TerPar        d06key
250100030926     c***********        movel     trul06ds      kpjbu
250200030926     c***********        call      'TRUL06R'
250300030926     c***********        parm                    kpjba
250400030926     c***********        movel     kpjbu         trul06ds
250500030926     c***********        movel     lin           £1
250600030926      *
250700030926     ** carico la schiera dei P.O presenti sull AS
250800030926     c                   clear                   tibs56ds
250900030926     c                   eval      i56ppo = simfel
251000030926     c                   eval      i56mod = 'POS'
251100030926     c                   call      'TIBS56R'
251200030926     c                   parm                    tibs56ds
251300030926     C                   MOVEA     SKI           Pou
251400980629      *
251500980223      * Caricamento Tabella Partner esteri
251600960528     C                   Z-ADD     0             X                 3 0
251700960528     C                   MOVEL     'PT'          TABCOD
251800960528     C     TABCOD        CHAIN     EDTAB01L                           30
251900960528     C     *IN30         DOWEQ     '0'
252000960528     C     TABFLG        IFEQ      *BLANKS
252100960528     C                   ADD       1             X
252200960528     C                   MOVEL     TABKEY        CPT(X)
252300961023     C                   MOVEL     TABKEY        CPU(X)
252400961021     C                   MOVE      '    '        CPT(X)
252500960528     C                   MOVEL     TABUNI        KSC(X)
252600960708     C                   MOVEL     TABUNI        DPT(X)
252700960528     C                   END
252800960528     C     TABCOD        READE     EDTAB01L                               30
252900960528     C                   END
253000980223      *
253100091117      * Deve comunque controllare che i codici caricati dalla PT
253200091117      *  non abbiano un altro codice di riferimento sulla 3K con cui
253300091117      *  impostare il membro.
253400091117     c                   do        x             xx
253500091117     C                   Z-ADD     1             KKUT
253600091117     C                   MOVEL     '3K'          KCOD
253700091117     C                   movel(p)  *zeros        KKEY
253800091117     C                   move      KSC(xx)       KKEY
253900091117     C     KTAB2         chain     tabel00f
254000091117     c                   if        %Found(tabel00f)
254100091117      *
254200091117     c                   movel     tbluni        ds3K
254300091117     c                   if        §3Kcks <> KSC(xx)
254400091117      *  sostituisce il codice con quello del membro
254500091117     c                   move      §3Kcks        Ksc(xx)
254600091117      *
254700091117     c                   end
254800091117     c                   end
254900091117     C                   END
255000980629      *
255100980629      *  In base al codice del PARTNER reperisco identificativo
255200080418     C******             MOVEL     MBR           WMBR              8
255300080418     C******             MOVE      WMBR          WCCM              7 0
255400980629      *
255500080418     C                   MOVE      Cliente       WCCM              7 0
255600020221     C                   Z-ADD     1             XX                3 0
255700980629     C     WCCM          LOOKUP    KSC(XX)                                32
255800020916      *
255900070503      *?  Se non trova la "PT" non deve proseguire   ?
256000070503     C     *IN32         IFEQ      '0'
256100070503     c                   move      'S'           wFINE
256200070503     c                   leaveSR
256300070503     c                   end
256400070503      *
256500980629     C     *IN32         IFEQ      '1'
256600980629      *
256700020412     C                   MOVEL     DPT(XX)       EDIDSPT
256800980629      *
256900980629     C                   MOVEL     'PU'          KCOD1
257000980707     C                   MOVEL     CPU(XX)       KKEY1
257100980707     C     KTABE         CHAIN     EDTAB01L                           30
257200980629      *
257300980629     C     *IN30         IFEQ      '0'
257400980629     C     TABFLG        ANDEQ     *BLANKS
257500020412     C                   MOVEL     TABUNI        EDIDSPU
257600980629     C                   END
257700980629      *
257800980629      * Caricamento Tabella codici eventi. Se richesta Lista Labarta e codici
257900980629      *   Labarta sono Blanks non carico rcd, lo stesso per Lista VGL.
258000980629      *
258100980629     C                   Z-ADD     0             X                 3 0
258200980629     C                   MOVEL     '2A'          TABCOD
258300980629      *
258400980629     C     TABCOD        CHAIN     EDTAB01L                           30
258500980629     C     *IN30         DOWEQ     '0'
258600020412     C                   MOVEL     TABUNI        EDIDS2A
258700980629      *
258800980629     C                   SELECT
258900980629      *
259000980629     C     TABFLG        WHENNE    *BLANKS
259100980629      *
259200980629     C     §PULAB        WHENEQ    'S'
259300980629     C     §2ASTS        ANDEQ     *BLANKS
259400980629     C     §2ACEV        ANDEQ     *BLANKS
259500980629      *
259600980629     C     §PULAB        WHENEQ    *BLANKS
259700980629     C     §2ASTV        ANDEQ     *BLANKS
259800980629     C     §2ACDV        ANDEQ     *BLANKS
259900980629      *
260000980629     C                   OTHER
260100980629     C                   ADD       1             X
260200980629     C                   MOVEL     TABKEY        C2A(X)
260300980629     C                   MOVEL     §2ACEV        CEV(X)
260400980629     C                   MOVEL     §2ASTS        STS(X)
260500980629     C                   MOVEL     §2ACDV        CDV(X)
260600980629     C                   MOVEL     §2ASTV        STV(X)
260700980629     C                   ENDSL
260800980629      *
260900980629     C     TABCOD        READE     EDTAB01L                               30
261000980629     C                   END
261100980629      *
261200980629     C                   END
261300980629      *---------------------------------------
261400980629      *
261500980223      * Caricamento NUMERATORE
261600960927     C                   EXSR      CARNUM
261700980223      * finta read x IFTSTA
261800960528     C   01
261900960528     CANN01              READ      EDIFTSTA                               01
262000980223      *
262100980223      * Controllo se esiste EDRDE
262200980126     C                   Z-ADD     45            LUNG             15 5
262300980126     C                   MOVEL     *BLANKS       COMMAN
262400980126     C                   MOVEA     CMD(1)        COMMAN           80
262500980126     C                   CALL      'QCMDEXC'                            21
262600980126     C                   PARM                    COMMAN
262700980126     C                   PARM                    LUNG
262800980126     C     *IN21         IFEQ      '0'
262900980126     C                   OPEN      EDRDE01L
263000980126     C                   MOVEL     'S'           WTRRDE            1
263100980126     C                   END
263200980223      *
263300960528     C                   ENDSR
263400980223      *-----------------------------------------------------*
263500980223      *   EXCPT   x aggiornamento flag trasmissione BLT     *
263600980223      *-----------------------------------------------------*
263700970303     OFNBLT000  E            AGGTRA
263800970303     O                       BLTFTR
263900990607      *-----------------------------------------------------*
264000990607      *   EXCPT   x aggiornamento flag trasmissione DCT     *
264100990607      *-----------------------------------------------------*
264200990607     OFNDCT000  E            AGGDCT
264300990607     O                       DCTFLO
264400980126**
264500980126CHKOBJ OBJ(EDRDE01L) OBJTYPE(*FILE)
