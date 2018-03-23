000100930128      ***********************************************************************
000200110318      *   ESPORTA STATO DELLE CONSEGNE VERSO E.D.I.   ESTERO                *
000300131014      *?                COMPILARE IN *CALLER  ?                             *
000400930128      ***********************************************************************
000500131014      *
000600131014     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700131014      *
000800930128     H DECEDIT('0,') DATEDIT(*DMY.)
000900131014      *
001000960528     F*---------------------------------------------------------------------*
001100960528     F*  DATA BASE                                                          *
001200960528     F*---------------------------------------------------------------------*
001300050120     Fazorg01l  IF   E           K DISK
001400050120     F*---------
001500080418     FTivgd01L  uF   E           K DISK
001600100304     F                                     INFDS(vgdDS)
001700100304     FTivgd00f  uF   E             DISK    RENAME(TIVGD000:TIVGDfis)
001800960528     F*---------
001900050309     Ftigcp01L  IF   E           K DISK
002000961204     F*---------
002100050309     Ftignp01L  IF   E           K DISK
002200960528     F*---------
002300970319     FFNLBL01L  IF   E           K DISK
002400970319     F*---------
002500970319     FFNLBL02L  IF   E           K DISK
002600970319     F                                     RENAME(FNLBL000:FNLBL002)
002700970319     F*---------
002800060213     Ffiar401L  IF   E           K DISK
002900960528     F*---------
003000960528     FFNBLP01L  IF   E           K DISK
003100960528     F*---------
003200970304     FFNBLT01L  UF   E           K DISK
003300970319     F*---------
003400051017     FFIARS01L  IF   E           K DISK
003500980219      *---------
003600990810     FFNEVB04L  IF   E           K DISK
003700980204     F*---------
003800990810     FFNEVB05L  IF   E           K DISK
003900990810     F                                     RENAME(FNEVB000:FNEVB005)
004000960528     F*---------
004100110405     FEDIFTSTA  iF A E             DISK    usropn
004200960628     F*---------
004300980204     FEDRDE01L  UF   E           K DISK    USROPN
004400960528     F*---------
004500960726     FEDTAB01L  UF   E           K DISK
004600961029     F*---------
004700961029     FTABEL00F  IF   E           K DISK
004800990604     F*---------
004900040715     FFNDCT01L  UF   E           K DISK
005000990604     FFNDCD01L  IF   E           K DISK
005100040607      *--
005200040607      *   x status 20/210 -> Supermercato
005300140221     Ffiarbf2c  IF   E           K DISK    prefix(G_)
005400040607      *--
005500100304      *
005600100304      * numero relativo di record
005700100304     D  vgdDS          DS
005800100304     D  NrRECvgd             397    400B 0
005900100304      *
006000960528     D*---------------------------------------------------------------------*
006100960528     D* Schiere
006200960528     D*---------------------------------------------------------------------*
006300960528     D* Schiera per scrittura dati località record ST10
006400960528     D* Schiera per scrittura testi liberi record ST15
006500960528     D* Schiera per scrittura testi liberi record SD15
006600960528     D* Schiera per reperimento nr.seganacollo
006700970625     D SG1             S             35    DIM(5000)                            Sgn.conse.
006800970625     D SG2             S             35    DIM(5000)                            Sgn.no con.
006900970625     D SG3             S             35    DIM(5000)                            Sgn.eve.pid
007000970625     D SG4             S             35    DIM(5000)                            IDD NO FL1
007100960528     D* Tabella codici eventi
007200961022     D CEV             S              3    DIM(200)
007300961022     D STS             S              3    DIM(200)
007400961022     D CDV             S              3    DIM(200)
007500961022     D STV             S              3    DIM(200)
007600960528     D C2A             S              3    DIM(200)
007700970319     D* Codici eventi IDD
007800970319     D IDD             S              3    DIM(200)
007900050909     D* Codici Giacenze/Lasciati Avvisi
008000050922     D GiaC            S              3    DIM(200)
008100050926     D LAvv            S              3    DIM(200)
008200960528     D* Tabella partner
008300070503     D CPT             S             35    DIM(300)
008400070503     D CPU             S             35    DIM(300)
008500070503     D DPT             S             90    DIM(300)
008600140221     D DPS             S             90    DIM(300)
008700070503     D KSC             S              7  0 DIM(300)
008800960821     D* TESTO LIBERO
008900970318     D***                 FTX     1   1 70
009000960912     D* composizione schiera 35 chr x segnacollo
009100960912     D C35             S              1    DIM(35)
009200030926     D  Pou            s              3  0 INZ
009300030926     D                                     DIM(250)
009400030926
009500980126     D* Esecuzione QCMD x controllare se esiste un CHKOBJ
009600990607     D CMD             S             45    DIM(1) CTDATA PERRCD(1)              QCAEXEC
009700960528     D*---------------------------------------------------------------------*
009800960528     D* DS
009900960528     D*---------------------------------------------------------------------*
010000960528     D* .. per scompozione dati da spedire a seconda del tipo record
010100960528     D EDST00        E DS                  EXTNAME(EDST00DS)
010200960708     D EDST05        E DS                  EXTNAME(EDST05DS)
010300960528     D EDST10        E DS                  EXTNAME(EDST10DS)
010400960528     D  T10                   48    103
010500960528     D                                     DIM(2)
010600960528     D EDST15        E DS                  EXTNAME(EDST15DS)
010700960528     D  T15                   13    362
010800960528     D                                     DIM(5)
010900960528     D EDSD00        E DS                  EXTNAME(EDSD00DS)
011000960528     D EDSD05        E DS                  EXTNAME(EDSD05DS)
011100960528     D EDSD10        E DS                  EXTNAME(EDSD10DS)
011200960528     D EDSD15        E DS                  EXTNAME(EDSD15DS)
011300960528     D  D15                   13    362
011400960528     D                                     DIM(5)
011500960528     D EDSD20        E DS                  EXTNAME(EDSD20DS)
011600960730     D  D20                   33    732
011700960730     D                                     DIM(20)
011800960528     D*  Ds per scrittura dati località
011900960528     D WST10           DS
012000960528     D  WTPL                   1      3
012100960528     D  WLOC                   4     28
012200960528     D*---------------------------------------------------------------------*
012300960528     D* .. per reperimento tipo record di EDIFTSTA da scrivere
012400960528     D*                    .. 5 - 8  : tipo record
012500040218     D WDAT            DS          1950
012600960729     D  STAPRG                 1      4  0
012700960726     D  STATPR                 5      8
012800960528     D*---------------------------------------------------------------------*
012900080418     D fnVACds       E DS
013000960528     D KPJBA         E DS
013100020412     D UT§DSE0F      E DS
013200960528     D CNCR80        E DS
013300050120     D OG143         E DS
013400050120     D ds1B          E DS
013500091117     D ds3K          E DS
013600020412     D EDIDS2A       E DS
013700970319     D DS2A          E DS
013800110318      **
013900020412     D EDIDSPT       E DS
014000020412     D EDIDSPU       E DS
014100110318     D EDIDSPV       E DS
014200110318     D EDIDSPZ       E DS
014300110318     D EDIDSPW       E DS
014400140221     D EDIDSPS       E DS
014500110318      **
014600020412     D EDIDSMS       E DS
014700020412     D DDCT01        E DS
014800960528     D*  DS x scomposizione segnacollo
014900960528     D DSSGN           DS
015000961029     D  SGNFLS                 1      3  0
015100960528     D  SGNLNA                 4      6  0
015200960528     D  SGNNRS                 7      8  0
015300960528     D  SGNNSC                 9     15  0
015400960829     D  SGNZNC                16     17  0
015500960528     D WLBDA8          DS
015600960528     D  G02DAT                 1      8  0
015700960528     D  G02INV                 9     16  0
015800960528     D  G02ERR                17     17
015900960528     D  G02TGI                18     22  0
016000990604      *
016100991019      * DS numero spedizione (da impostare x Europolitan)
016200980122     D DATSPE          DS
016300040305     D  CNI_SPE                3     16  0
016400080418     D   cni_VACAAS            1      4  0
016500080418     D   cni_VACLNP            5      7  0
016600080418     D   cni_VACNRS            8      9  0
016700080418     D   cni_VACNSP           10     16  0
016800080418     D   cni_VACLNA           17     19  0
016900980122     D  FILLER                20     20
017000040715
017100040715     d Fidn12ds      e ds
017200040715     d  skKey                 26    305    dim(20)
017300040715     d  skAnn                306    325    dim(20)
017400040715     d  skDca                326    485    dim(20)
017500040715     d  skFca                486    545  0 dim(20)
017600040715     d  skDch                546    705  0 dim(20)
017700040715     d  skCch                706    745    dim(20)
017800040715
017900040715     d dsKey           ds
018000040715     d  dsaac                         4  0
018100040715     d  dsfil                         3  0
018200040715     d  dsnca                         7  0
018300040715
018400040715     D Kaac            S                   LIKE(dctaac)
018500040715     D Kfil            S                   LIKE(dctfil)
018600040715     D Knca            S                   LIKE(dctnca)
018700140221     D salva_data      S                   LIKE(G_arbdtv)
018800140221     D salva_ora       S                   LIKE(G_arborv)
018900020418
019000030926     D Tibs56ds      E DS
019100030926     D                                     INZ
019200030926     D  Ski                           3  0
019300030926     D                                     DIM(250)
019400030926     D                                     OVERLAY(O56Ski)
019500030926
019600040607     d variata         s              1    INZ('N')
019700081112     d Bolla_variata   s              1    INZ('N')
019800110630      *----------------------------------------------------*
019900110630      *  Invio E-mail
020000110630     D Indirizzi       s            253
020100110630     D Oggetto         s             44
020200110630     D Messaggio       s           5000
020300110630      *
020400110630     d   DSmail        ds
020500110630     D Mail_Ind                      44
020600110630     d   IND_char                     1    dim(44)
020700110630      *
020800110630     D Lunghezza_Indirizzo...
020900110630     D                 s              5u 0
021000140221      *
021100140221     D Invio_Appuntamento_Supermercato...
021200140221     D                 s              1a
021300140416     D Invio_RFF_CU...
021400140416     D                 s              1a
021500160706     D ordinamSTD_EDIFACT_FTX_NAD...
021600160706     D                 s              1a
021700161013     D Delivery_STATUS...
021800161013     D                 s              1a
021900161013     D Invio_NAD_CN...
022000161013     D                 s              1a
022100161013     D Invio_NAD_CN_Solo_in_Delivery...
022200161013     D                 s              1a
022300110630     C*---------------------------------------------------------------*
022400110630     D MsgDSP          S            256                                         Testo generico
022500110630     C*---------------------------------------------------------------*
022600110630     D SndMg01         S             10                                         Message type
022700110630     D                                     INZ('*INFO')
022800110630     D SndMg02         S             10                                         Deluvery mode
022900110630     D                                     INZ('*BREAK')
023000110630     D SndMg03         S            256                                         Message text
023100110630     D SndMg04         S             10I 0                                      Length of text
023200110630     D                                     INZ(%SIZE(SndMg03))
023300110630     D SndMg05         S             10                                         User profile
023400110630     D                                     DIM(299)
023500110630     D SndMg06         S             10I 0                                      Number of user
023600110630     D SndMg07         S             10I 0                                      Message sent indic.
023700110630     D SndMg08         S             10I 0                                      Function requested
023800110630     D SndMg10         S              1                                         Show display
023900110630     D                                     INZ('N')
024000110630     D SndMg11         S             20                                         Qualified MSGQ name
024100110630     D SndMg12         S              4                                         Name type indicator
024200110630     D                                     INZ('*USR')
024300110630      * indice di scihera
024400110630     D Jx              S              5I 0
024500110630      *
024600110630     D/COPY QSYSINC/QRPGLESRC,QUSEC
024700110630      *
024800120830     d bart_it         C                   CONST('@Brt.it;')
024900120830     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
025000020418
025100960708     D*
025200961122     D COST1           C                   CONST('IT/BAR/IT1   ')
025300060503     D CALB_Plat       C                   CONST('039IT04507990150')
025400960528     I*---------------------------------------------------------------------*
025500960528     I* FILE
025600960528     I*---------------------------------------------------------------------*
025700960528     IIFTSTA00
025800960528     I              SSIFTSTA                    STADAT
025900040607
026000040607     I* GLI INDICATORI DI TIPO RECORD x file Combinato
026100040607     IFNARBD00      11
026200040607     IFNARBK00      12
026300040607     IFIARBT00      13
026400040607     IFNARBG00      14
026500040607     IFNARBM00      15
026600040607     IFNARBV00      16
026700040607     IFNARBP00      17
026800131014     IFNARBN00      18
026900040607      *  INDICI PER:
027000040607      *  - ANNO
027100040607      *  - LINEA PARTENZA
027200040607      *  - SERIE
027300040607      *  - NUMERO SPEDIZIONE
027400040607      *  - DATA VARIAZIONE
027500040607      *  - ORA VARIAZIONE
027600000201      *---------------------------------------------------------------------*
027700000201      * Ciclo principale
027800000201      *---------------------------------------------------------------------*
027900070503      *   Se non è stata trovata la PT nella INZSR deve chiudere il PGM  ?
028000070503     C                   if        WFINE = 'S'
028100070503     c                   goto      fine
028200070503     c                   end
028300080418      * Posizionamento sul TIVGD x il nuovo ciclo di lettura
028400080418     C                   EXSR      POSFIL
028500000201      *
028600960528      * Posizionamento iniziale sul file  (prima lettura)
028700960528     C                   EXSR      REDFIL
028800000201      *
028900000201      * TESTATA Messaggio
029000000201     C                   if        WFINE <> 'S'
029100000201     C                   EXSR      TESTAMSG
029200000201     C                   endif
029300980220      *
029400980220      * Loop letture FNVAC e scrittura record con dati singola bolla
029500000201     C     WFINE         DOWNE     'S'
029600000201      *
029700000201      * Se ho scritto 999 spedizioni devo scrivere un nuovo msg
029800000201     C                   if        WPROG = 999
029900980223      * Caricamento NUMERATORE
030000960927     C                   EXSR      CARNUM
030100000201      * TESTATA Messaggio
030200000201     C                   EXSR      TESTAMSG
030300000201     C                   endif
030400000201      *
030500000201      *  DETTAGLIO Messaggio
030600000201      *
030700980220      * Eseguo preparazione dati per scrittura record SD00
030800960528     C                   EXSR      WRSD00
030900980220      * Eseguo scrittura dati record SD05 e SD00
031000980220     C                   MOVEL     'N'           WRTEVE            1
031100960528     C                   EXSR      WRSD05
031200980220      *
031300980220      * Scrivo tipi record successivi solo se ho scritto SD00 e SD05
031400980220     C     WRTEVE        IFEQ      'S'
031500150611      *
031600150611      *   Vero Standard in sequenza corretta in prova solo su cliente SDKCOS
031700160706      *    e PARTNER France Express.  (prima deve essere segm FTX poi NAD+AP:Firma)
031800160706     C                   if        ordinamSTD_EDIFACT_FTX_NAD = 'S'
031900150611     C                   EXSR      WRSD15
032000150611     C                   EXSR      WRSD10
032100150611     c                   else
032200150611      *
032300980220      * Eseguo scrittura dati record SD10
032400960528     C                   EXSR      WRSD10
032500960528      * Eseguo scrittura dati record SD15
032600960528     C                   EXSR      WRSD15
032700150611     c                   end
032800150611      *
032900960528      * Eseguo scrittura dati record SD20
033000960528     C                   EXSR      WRSD20
033100980220     C                   END
033200100304      *
033300100304      * Se arrivato qui tutto OK
033400100304      * Aggancia il record fisico per certificare che è andato tutto bene
033500100304      *  e che ha elaborato correttamente il record trasformandolo in EDI.
033600100304     c                   clear                   elab10           10 0
033700100304     c     NrRECvgd      chain     tiVGD00F
033800100304     c                   if        %Found(tiVGD00F)
033900100304     c                   eval      Elab10 = WoggiYMD * 10000 + wHHMM
034000100304     c                   movel     Elab10        vgdPRG
034100100304     c                   update    tiVGDfis
034200100304     c                   end
034300980220      * Lettura successiva
034400961007     C     NEWREC        TAG
034500960528     C                   EXSR      REDFIL
034600000201     C                   ENDDO
034700980220      * Fine pgm.
034800121010      *   se si tratta di un traduttore NON EDIFACT deve far saltare al CL la parte
034900121010      *    successiva di trasmissione
035000121010     c                   if        §PZPGMTRSS = 'GELSTA'
035100131003     c                                or
035200131003     c                             §PZPGMTRSS = 'AMZSTA'
035300121010     c                   eval      Righe_Dett = 'N'
035400121010     c                   end
035500121010      *
035600070503     c     fine          tag
035700000201     C                   eval      *inlr = *on
035800080418      *----------------------------------------------------------------
035900080418      *? POSFIL - Si Posiziona x la LETTURA PRIMO RECORD
036000080418      *----------------------------------------------------------------
036100080418     C     POSFIL        BEGSR
036200080418      *
036300080418      *? Si posiziona con il tipo VC e cliente + l'invio EDI...  ?
036400080418     C     KEY_VGD01     setll     tiVGD01L
036500080418      *
036600080418     C                   ENDSR
036700980220      *----------------------------------------------------------------
036800980220      *? REDFIL - LETTURA PROSSIMO RECORD
036900980220      *----------------------------------------------------------------
037000960528     C     REDFIL        BEGSR
037100980220      *
037200990326     C     LEGGI         TAG
037300990326      *
037400080418      * Lettura file VAC
037500080418     C     KEY_VGD01     reade     tiVGD01L                               30
037600990326      *
037700000201     C                   If        *IN30 = *on
037800000201     C                   MOVEL     'S'           WFINE             1
037900110318      *
038000110318     c                   If        §PZPGMTRSS = 'STA94A'
038100110322     C                               and  Numero_Righe > 1
038200120830     c                               or
038300120830     c                             §PZPGMTRSS = 'GELSTA'
038400131003     c                               or
038500131003     c                             §PZPGMTRSS = 'AMZSTA'
038600120830      *
038700110405      * Se c'è stato qualche evento da inviare che quindi ha scritto
038800110405      *   chiude il messaggio altrimenti esegue un ROLLBACK
038900110405     c                   if        Righe_Dett = 'S'
039000120830      *
039100110318      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
039200110318      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
039300110412     c                   move      §MSNUM        fine04            4
039400110318     c                   eval      STATPR = 'FINE'
039500110411     c                   eval      STADAT = fine04
039600120830     c                   exsr      write_OUTPUT
039700110318     c                   eval      Righe_Dett = 'N'
039800110405      *
039900110405     c                   else
040000110405      * deve uscire senza avere fatto nulla
040100110405     c                   eval      STATPR = 'RLBK'
040200120830     c                   exsr      write_OUTPUT
040300110405     c                   end
040400110405      *
040500110318     c                   end
040600121010      *
040700000201     C                   Else
040800090219      *
040900090219      *? DEVE LEGGERE solo i record con TIPO invio "EW" da mandare via EDI
041000090219     c                   if        vgdTSC <> 'EW'
041100090219     c                   goto      LEGGI
041200090219     c                   end
041300990326      *
041400080418      *? Una volta letto il TIVGD imposta la relativa DS del VAC...  ?
041500080418     c                   eval      fnVACds = vgdDTA
041600080418      *
041700080418      *? poi aggiorna come già letto il record ...  ?
041800080418     c                   eval      VGDSTO ='S'
041900100304     c                   eval      VGDCNT = VGDCNT + 1
042000080418      *
042100080418      *?                ====================  ?
042200080418     c                   update    tivgd000
042300080418      *?                ====================  ?
042400080418      *
042500051116      *  Pulizia campi riferimento di giacenze della precedente lettura
042600051116     c                   clear                   GCPAGC
042700051116     c                   clear                   GCPFGC
042800051116     c                   clear                   GCPNGC
042900051116      *
043000990326      * Non elaboro codice anomalia IDD3 trasmesso dall'arrivo
043100000201     C                   if        VACCCA = '5'
043200990326     C                   GOTO      LEGGI
043300000201     C                   endif
043400990922      *
043500990922      * Non elaboro SE NON TROVO RCD DI FNBLP
043600990922     C                   Z-ADD     VACAAS        KAAS
043700990922     C                   Z-ADD     VACLNP        KLNP
043800990922     C                   Z-ADD     VACNRS        KNRS
043900990922     C                   Z-ADD     VACNSP        KNSP
044000990922     C     KBLP          CHAIN     FNBLP01L                           31
044100000201     C   31              GOTO      LEGGI
044200000201      *
044300000201     C                   Endif
044400980220      *
044500960528     C                   ENDSR
044600000201      *----------------------------------------------------------------
044700000201      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO
044800000201      *----------------------------------------------------------------
044900000201     C     TESTAMSG      BEGSR
045000000201      *
045100000201      * Eseguo scrittura dati record ST00
045200000201     C                   EXSR      WRST00
045300000201      * Eseguo scrittura dati record ST05
045400000201     C                   EXSR      WRST05
045500000201      * Eseguo scrittura dati record ST10
045600000201     C***                EXSR      WRST10
045700000201      * Eseguo scrittura dati record ST15
045800000201     C                   EXSR      WRST15
045900000201      *
046000000201     C                   ENDSR
046100980629      *----------------------------------------------------------------
046200980629      *? WRST00 - Scrittura record partner
046300980629      *----------------------------------------------------------------
046400960528     C     WRST00        BEGSR
046500980629      *
046600980629      *  Pulisco dati DS
046700960528     C                   CLEAR                   EDST00
046800980629      *  Mittente: Bartolini  (ITBAR + AS..)
046900960528     C                   MOVEL     'ITBAR'       WMITT             8
047000960917     C                   MOVE      '005'         WMITT             8
047100960530     C                   MOVEL     WMITT         TA0004
047200960729     C                   MOVEL     'ZZ'          TA0007
047300980220      *
047400980629      *  In base al codice del PARTNER reperisco identificativo
047500980629     C                   MOVEL     CPT(XX)       TA0010
047600980629      *
047700980220      *  Utilizzo qualificatore x destinatario msg.
047800960827     C                   MOVEL     §PTMSQ        TAA007
047900980220      *  Data/Ora invio messagio: adesso
048000971031     C                   MOVEL     WOGGI         TA0017
048100960528     C                   MOVEL     WHHMM         TA0019
048200980220      *  Codice documento - nome documento
048300960528     C                   MOVEL     '784'         TA1001
048400960913     C                   MOVEL     '      '      TA1000
048500960708     C                   MOVEL     WHHDAT        TA1004                         DATA/ORA -> MSG
048600960528     C                   MOVEL     '9  '         TA1225
048700960528     C                   MOVEL     '137'         TA2005
048800980220      *  Data messaggio: oggi
048900960528     C                   MOVEL     WOGGI         TA2380
049000960528     C                   MOVEL     '102'         TA2379
049100100512      *
049200100512      *    Se si vuole la data con l'orario
049300100512     c                   if        §pudate203 ='S'
049400100512     C                   MOVE      Whhmm         TA2380
049500100512     C                   MOVEL(p)  '203'         TA2379
049600100512     c                   end
049700100512      *
049800980220      *  Scivo record IFTSTA
049900960705     C                   CLEAR                   WDAT
050000960705     C                   MOVEL     EDST00        WDAT
050100960726     C                   Z-ADD     §MSNUM        STAPRG
050200960726     C                   MOVEL     'ST00'        STATPR
050300110318     C                   MOVEL     WDAT          STADAT
050400120830      *
050500120830      *  Testata messaggio  UNB-BGM-DTM
050600120830     c                   exsr      write_OUTPUT
050700110315      *
050800960528     C                   ENDSR
050900980220      *----------------------------------------------------------------
051000980220      *? WRST05 - Scrittura record partner
051100980220      *----------------------------------------------------------------
051200960708     C     WRST05        BEGSR
051300980220      *
051400960708     C                   CLEAR                   EDST05
051500980220      *  Ship from:
051600960712     C                   MOVEL     *BLANKS       WDAT
051700960708     C                   MOVEL     'SF '         TE3035
051800970221     C                   MOVEL     §PTPLT        TE3036
051900980220      *  Scivo record IFCSUM
052000960708     C                   CLEAR                   WDAT
052100960708     C                   MOVEL     EDST05        WDAT
052200960726     C                   Z-ADD     §MSNUM        STAPRG
052300960712     C                   MOVEL     'ST05'        STATPR
052400960708     C                   MOVEL     WDAT          STADAT
052500120830      *
052600120830      *  Testata messaggio  NAD-SF
052700120830     c                   exsr      write_OUTPUT
052800980220      *  Ship to:
052900960712     C                   MOVEL     *BLANKS       WDAT
053000960708     C                   CLEAR                   EDST05
053100960708     C                   MOVEL     'ST '         TE3035
053200970221     C                   MOVEL     COST1         TE3036
053300060503      *
053400060503      *  x CALBERSON --> GEODIS
053500060503      *      imposta la Platform che ci hanno chiesto di utilizzare
053600060503      *      x identificare diversamente dai dati di DUNLOP altrimenti
053700060503      *      non riescono a dividerli nel loro sistema.
053800060503      *  da impostare il §PTLOC ??????
053900060503     c                   if        §PTKSC = 493298
054000060503     C                   clear                   TE3036
054100060503     C                   MOVEL     CALB_Plat     TE3036
054200060503     c                   end
054300060503      *
054400980220      *  Scivo record IFCSUM
054500960708     C                   CLEAR                   WDAT
054600960708     C                   MOVEL     EDST05        WDAT
054700960726     C                   Z-ADD     §MSNUM        STAPRG
054800960712     C                   MOVEL     'ST05'        STATPR
054900960708     C                   MOVEL     WDAT          STADAT
055000120830      *
055100120830      *  Testata messaggio  NAD-ST
055200120830     c                   exsr      write_OUTPUT
055300980220      *
055400960708     C                   ENDSR
055500980220      *----------------------------------------------------------------
055600980220      *? WRST10 - Scrittura record partner
055700980220      *----------------------------------------------------------------
055800960528     C     WRST10        BEGSR
055900980220      *
056000980220      *  Pulisco dati DS
056100960712     C                   MOVEL     *BLANKS       WDAT
056200960528     C                   CLEAR                   EDST10
056300980220      *  Se devo ritornare imposto AFB + nr.messaggio
056400960726     C                   MOVEL     'AFB'         TB1153
056500960726     C                   MOVEL     §MSNUM        TB1154
056600980220      *  Scivo record IFCSUM
056700960726     C                   CLEAR                   WDAT
056800960726     C                   MOVEL     EDST10        WDAT
056900960726     C                   Z-ADD     §MSNUM        STAPRG
057000960726     C                   MOVEL     'ST10'        STATPR
057100960726     C                   MOVEL     WDAT          STADAT
057200120830      *
057300120830      *  Testata messaggio  RFF-LOC
057400120830     c                   exsr      write_OUTPUT
057500980220      *
057600960528     C                   ENDSR
057700980220      *----------------------------------------------------------------
057800980220      *? WRST15 - Scrittura record partner
057900980220      *----------------------------------------------------------------
058000960528     C     WRST15        BEGSR
058100980220      *
058200980220      *  Pulisco dati DS
058300960712     C                   MOVEL     *BLANKS       WDAT
058400960528     C                   CLEAR                   EDST15
058500980220      *
058600960528     C                   ENDSR
058700980220      *----------------------------------------------------------------
058800980220      *? WRSD00 - Scrittura record partner - dettaglio
058900980220      *----------------------------------------------------------------
059000960528     C     WRSD00        BEGSR
059100980220      *
059200980220      *  Pulisco dati DS
059300960712     C                   MOVEL     *BLANKS       WDAT
059400960528     C                   CLEAR                   EDSD00
059500980220      *  Controllo se la bolla figlia con bolla originale su stesso AS
059600980220      *  non invio le date di consegna
059700970319     C                   Z-ADD     VACAAS        KAAN
059800970319     C                   Z-ADD     VACLNP        KLPN
059900970319     C                   Z-ADD     VACNRS        KNRN
060000970319     C                   Z-ADD     VACNSP        KNSN
060100970319     C     KLBL1         CHAIN     FNLBL01L                           31
060200970319     C     *IN31         IFEQ      '0'
060300030926     C     LBLLPO        LOOKUP    Pou                                    31
060400970319     C   31              GOTO      NEWREC
060500970319     C                   END
060600980220      *
060700980220      *  Imposto numero spedizione originale cliente/partner
060800961022     C                   MOVEL     *BLANKS       WNRSPE           35
060900960528     C                   Z-ADD     VACAAS        KAAS
061000960528     C                   Z-ADD     VACLNP        KLNP
061100960528     C                   Z-ADD     VACNRS        KNRS
061200960528     C                   Z-ADD     VACNSP        KNSP
061300040305      * La regola è:
061400040305      *  si deve restituire il Nostro Nr.spedizione nel CNI invece
061500060213      *   dal ar4 --> deve essere impostato l'RFF+AGE ossia Rif.Partner
061600080418     c                   eval      cni_VACAAS = VACAAS
061700080418     c                   eval      cni_VACLNP = VACLNP
061800080418     c                   eval      cni_VACNRS = VACNRS
061900080418     c                   eval      cni_VACNSP = VACNSP
062000080418     c                   eval      cni_VACLNA = VACLNA
062100040305      *
062200040305      * quindi occorre impostare diversamente i campi della DS00 e DS05
062300960708     C                   MOVEL     'E'           KTRC
062400060213     C     Kar4          CHAIN     fiar401L                           31
062500980220      *
062600100722      * se non richiesta forzatura del Riferimento numerico da restituire
062700100722     c                   if        *in31 = '0' and §ptSGN =' '
062800060213     C                   MOVEL     ar4NOT        Wnrspe
062900960821     C                   ELSE
063000980220      *
063100060213      * Se non trovo ar4NOT utilizzo in cascata Rif.Mittente Alfa e numerico
063200980220      * (????) Segnalo se manca ricezione EDI e sped. bollettata a mano
063300980220     C                   MOVEL     'S'           WSD15             1
063400960821     C     KBLP          CHAIN     FNBLP01L                           31
063500960821     C     *IN31         IFEQ      '0'
063600100722      * se non richiesta forzatura del Riferimento numerico da restituire
063700960821     C     BLPRMA        IFNE      *BLANKS
063800100722     C     §ptsgn        andeq     *BLANKS
063900040305     C                   MOVEL     BLPRMA        Wnrspe
064000960821     C                   ELSE
064100040305     C                   MOVEL     BLPRMN        Wnrspe
064200961007     C                   ENDIF
064300960821     C                   ENDIF
064400960708     C                   END
064500100209      * Default
064600100209      *  se non richiesta l'inversione del dato CNI nell'AGE
064700100209     c                   if        §ptTRD <> 'S'
064800100209     C                   MOVEL(p)  CNI_spe       DA1004
064900100209      *  inverte con l'AGE
065000100209     c                   elseIF    §ptTRD  = 'S'
065100100209     C                   MOVEL(p)  Wnrspe        DA1004
065200100209     c                   end
065300100304      *
065400100304      * Se non ho impostato DA1004 leggo rcd successivo
065500100304     C     DA1004        IFEQ      *BLANKS
065600100304     C                   GOTO      NEWREC
065700100304     C                   ENDIF
065800980220      *
065900161013      *--------------
066000161013      *  Pulizia campi utilizzati successivamente
066100161013     c                   eval      Delivery_STATUS = *blank
066200161013      *
066300161013      *--------------
066400960528     C                   ENDSR
066500980220      *----------------------------------------------------------------
066600980220      *? WRSD05 - Scrittura record partner - dettaglio
066700980220      *----------------------------------------------------------------
066800960528     C     WRSD05        BEGSR
066900980220      *
067000980220      *  Pulisco dati DS
067100960712     C                   MOVEL     *BLANKS       WDAT
067200960528     C                   CLEAR                   EDSD05
067300980220      *
067400980220      *  Se ho l'obbligo di ritornaglielo scrivo nr. CMR altrimenti nr. sped.
067500980220     C     §PURFF        IFEQ      'C'
067600980220     C                   EXSR      NUMCMR
067700980220     C                   ELSE
067800961022     C                   MOVEL     'AGE'         DB1153
067900100209      * Default
068000100209      *  se non richiesta l'inversione del dato CNI nell'AGE
068100100209     c                   if        §ptTRD <> 'S'
068200961022     C                   MOVEL     WNRSPE        DB1154
068300100209      *  inverte con il CNI
068400100209     c                   elseIF    §ptTRD  = 'S'
068500100209     C                   MOVEL     CNI_spe       DB1154
068600100209     c                   end
068700140416      *
068800980220     C                   END
068900140416      *------------------------------------------------------*
069000140416      *   se si deve inviare forzatamente
069100140416      *     il riferimento numerico RFF+CU oltre all'AGE
069200140416      *  NON è in alternativa al riferimento CMR è sempre in più
069300140416      **
069400140416     C                   if        Invio_RFF_CU = 'S'
069500140416     C                               and BLPrmn > 0
069600140416     C                   eval      DB1153A = 'CU'
069700140416     C                   eval      DB1154A = %Trim(%Editc(blpRMN:'Z'))
069800140416     c                   end
069900980220      *------------------------------------------------------*
070000980220      *  Se la bolla è una mamma controllo se è stato effettuato un evento IDD
070100970319     C                   MOVEL     'N'           WTREVE            1
070200970319     C                   Z-ADD     VACAAS        KAAP
070300970319     C                   Z-ADD     VACLNP        KLPP
070400970319     C                   Z-ADD     VACNRS        KNRP
070500970319     C                   Z-ADD     VACNSP        KNSP
070600970319     C     KLBL2         CHAIN     FNLBL02L                           31
070700980220      *
070800980220      *  Scorro schiera eventi IDD e controllo se ce ne sono
070900980220     C     *IN31         IFEQ      '0'
071000970319     C                   Z-ADD     1             X
071100980220     C     IDD(X)        DOWNE     *BLANKS
071200970319     C     X             ANDLE     200
071300970319     C     WTREVE        ANDEQ     'N'
071400970319     C                   MOVEL     IDD(X)        KCEV
071500970319     C                   Z-ADD     VACAAS        KAA2
071600970319     C                   Z-ADD     VACLNP        KLNP
071700970319     C                   Z-ADD     VACNRS        KNRS
071800970319     C                   Z-ADD     VACNSP        KNSP
071900990810     C     KEVB          CHAIN     FNEVB04L                           31
072000980220     C  N31              MOVEL     'S'           WTREVE
072100980220     C   31              ADD       1             X
072200980220     C                   END
072300980220      *
072400980220     C                   END
072500980220      *------------------------------------------------------*
072600990604      *  APERTURA C.A.
072700990604     C                   MOVEL     *BLANKS       WELACA            1
072800990604     C     VACCCA        IFEQ      'A'                                          -- 01 --
072900040715      *
073000040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
073100040715      * SIA COME MAMMA CHE COME FIGLIA
073200040715     c                   clear                   fidn12ds
073300040715     c                   eval      i12aas = kAAS
073400040715     c                   eval      i12lnp = kLNP
073500040715     c                   eval      i12nrs = kNRS
073600040715     c                   eval      i12nsp = kNSP
073700040715     c                   eval      i12fel = 'E'
073800040715     c                   eval      i12fan = 'E'
073900040715     c                   eval      i12fch = 'E'
074000040715     c                   eval      i12fpt = 'E'
074100040715     c                   call      'FIDN12R'
074200040715     c                   parm                    fidn12ds
074300040715     c                   z-add     *zeros        ii                2 0
074400040715      **
074500040715      * se trovata almeno una CA
074600040715     c                   if        o12nca > 0
074700040715     c                   dow       ii <  o12nca
074800040715     c                   add       1             ii
074900040715     c                   movea     skkey(ii)     dskey
075000040715     C                   z-add     dsaac         kaac
075100040715     C                   z-add     dsfil         kfil
075200040715     C                   z-add     dsnca         knca
075300040715     c     kdct          chain(n)  FNDCT000
075400040715     c                   If        %found(fndct01l)
075500040715     C                   MOVEL     DCTFLO        DDCT01
075600040715     C     §DCTtrpt      ifeq      *BLANKS
075700040715     C                   MOVEL     'S'           WELACA
075800040715     C                   leave
075900040715     C                   endif
076000040715     C                   endif
076100040715      *
076200040715     c                   enddo
076300040715     c                   endif
076400990604      *
076500990604     C     WELACA        IFEQ      'S'                                          -- 02 --
076600990604      *  Determino la causale evento e controllo se reso codificato in tabella
076700991019      *   IDD per mancanze EuroExpress ed Europolitan; RFD per Avarie e Ammanchi
076800991019      *    EuroExpress; per Avarie e Ammanchi Europolitan non trasmetto nulla.
076900990604     C                   Z-ADD     1             X
077000991019     C                   SETOFF                                       32
077100990604      *
077200991019     C                   SELECT
077300991019      *
077400991019     C     DCTTAD        WHENNE    '01'                                         EuroExpress
077500991019     C     DCTTAD        ANDNE     '21'                                         mancanze
077600990604     C     'RFD'         LOOKUP    C2A(X)                                 32
077700991019      *
077800991019     C                   OTHER                                                  EuroExpress altro
077900990604     C     'IDD'         LOOKUP    C2A(X)                                 32
078000991019     C                   ENDSL
078100990604      *
078200990604     C     *IN32         IFEQ      '1'                                          -- 03 --
078300990604     C     §PULAB        IFEQ      'S'
078400990604     C                   MOVEL     CEV(X)        DB9013
078500990604     C                   MOVEL     STS(X)        DB9011
078600990604     C                   ELSE
078700990604     C                   MOVEL     CDV(X)        DB9013
078800990604     C                   MOVEL     STV(X)        DB9011
078900990604     C                   ENDIF
079000990604     C                   MOVEL     '7  '         DB2005
079100990604     C                   MOVEL     VACDCM        WDATHM
079200990604     C                   MOVE      VACHMC        WDATHM           12 0
079300990604     C                   MOVEL     WDATHM        DB2380
079400990604     C                   MOVEL     '203'         DB2379
079500990604      *  Scrivo record SD00 e SD05
079600990604     C                   EXSR      WRT0E5
079700990604     C                   ENDIF                                                  -- 03 --
079800990604      *
079900990604     C                   ENDIF                                                  -- 02 --
080000990608     C                   GOTO      FIND05
080100990604     C                   ENDIF                                                  -- 01 --
080200990604      *------------------------------------------------------*
080300980220      *  Controllo se è stata effettuata un consegna parziale
080400970319     C                   Z-ADD     VACAAS        KAAS
080500970319     C                   Z-ADD     VACLNP        KLNP
080600970319     C                   Z-ADD     VACNRS        KNRS
080700970319     C                   Z-ADD     VACNSP        KNSP
080800970319     C     KBLP          CHAIN     FNBLP01L                           31
080900980220      *
081000980220     C     BLPDCP        IFGT      0
081100960729     C     BLPDCM        ANDEQ     0
081200980220      *
081300980220      *  Controllo se devo trasmettere i colli
081400980220     C     WTREVE        IFEQ      'S'
081500970320     C                   EXSR      CHKBLT
081600980220      *  Se  non ci sono colli da trasmettere passo al record successivo
081700980220     C     WTRCOL        IFEQ      'N'
081800980220     C                   GOTO      NEWREC
081900980220     C                   END
082000980220     C                   END
082100980220      *
082200960528     C                   Z-ADD     1             X
082300970617     C                   SETOFF                                       32
082400980220      *
082500970617     C     §PTDET        IFEQ      'M'
082600970320     C     BLPCCA        ANDEQ     '5'
082700970320     C     §PTDET        OREQ      'M'
082800970320     C     WTREVE        ANDEQ     'S'
082900970320     C     'PID'         LOOKUP    C2A(X)                                 32
083000970320     C                   ELSE
083100970320     C     'P  '         LOOKUP    C2A(X)                                 32
083200970320     C                   END
083300980220      *
083400980220     C   32§PULAB        IFEQ      'S'
083500960528     C                   MOVEL     CEV(X)        DB9013
083600960612     C                   MOVEL     STS(X)        DB9011
083700961022     C                   ELSE
083800961022     C                   MOVEL     CDV(X)        DB9013
083900961022     C                   MOVEL     STV(X)        DB9011
084000980220     C                   END
084100980220      *
084200960528     C                   MOVEL     '7  '         DB2005
084300960528     C                   MOVEL     BLPDCP        DB2380
084400960528     C                   MOVEL     '102'         DB2379
084500100512      *
084600100512      *    Se si vuole la data con l'orario
084700100512     c                   if        §pudate203 ='S'
084800140910     C                   MOVE      '0001'        DB2380
084900140910     C                   MOVEL(p)  '203'         DB2379
085000140910      *
085100140910      *  Controlla direttamente sugli EVENTI la presenza di Data e Ora
085200140910      *   da impostare se richiesta l'informazione completa dell'orario.
085300140910     C                   eval         KCEV_P = 'P  '
085400140910      *-
085500140910     C     KParzial      setgt     fnEVB04L
085600140910     C     KParzial      readpe    fnEVB04L
085700140910     c                   dow       not %EoF(fnEVB04L)
085800140910     c                   if           evbDEV = blpDCP
085900140910     C                   movel     evbDEV        WDATHM
086000140910     C                   move      evbHEV        WDATHM
086100140910     C                   movel     WDATHM        DB2380
086200140910     c                   leave
086300140910     c                   end
086400140910     C     KParzial      readpe    fnEVB04L
086500140910     c                   enddo
086600140910      *
086700100512     c                   end
086800100512      *
086900980220      *  Scrivo record SD00 e SD05
087000980220     C                   EXSR      WRT0E5
087100960528     C                   GOTO      FIND05
087200980220     C                   END
087300990604      *-------------------------------------------------------------*
087400050922      * aggancia sempre la giacenza se c'è stata giacenza e non è consegnata
087500050922     C     vacDAG        ifGT      0                                             -- 0A --
087600050922     C     vacDCM        andEQ     0
087700050922     C                   Z-ADD     VACAAS        KAAS
087800050922     C                   Z-ADD     VACLNP        KLNP
087900050922     C                   Z-ADD     VACNRS        KNRS
088000050922     C                   Z-ADD     VACNSP        KNSP
088100050922     c                   Z-ADD     0             KFRG
088200050922     C     KGCP          CHAIN     tigcp01L                           31
088300050922     c                   if        %Found(tigcp01L)                              -- 0B --
088400050922
088500050922      * imposta la data Distinta/udate apertura giacenza fuori distinta
088600050922     c                   z-add     GCPDXD        vacDAG
088700050922
088800980220      *  Se la data più alta è di giacenza invio data apertura giacenza
088900970617     C     VACDAG        IFGT      VACDLA                                       -- 01 --
089000960528     C     VACDAG        ANDGT     VACDCM
089100960528     C     VACDAG        ANDGT     0
089200980220      *  Controllo se il codice apertura giacenza è codificato in tabella
089300960528     C                   Z-ADD     1             X
089400960528     C     GCPCMC        LOOKUP    C2A(X)                                 32
089500970617     C     *IN32         IFEQ      '1'                                          -- 02 --
089600980220      *
089700980220     C     §PULAB        IFEQ      'S'                                          -- 03 --
089800960528     C                   MOVEL     CEV(X)        DB9013
089900960612     C                   MOVEL     STS(X)        DB9011
090000980220     C                   ELSE                                                   -- 03 --
090100961022     C                   MOVEL     CDV(X)        DB9013
090200961022     C                   MOVEL     STV(X)        DB9011
090300980220     C                   END                                                    -- 03 --
090400980220      *
090401180223      * Per la GEODIS c'è un chiodo al momento sul codice "032"
090402180223      *  x il quale non si deve inviare il 56+265 generico ma un codice
090403180223      *    inventato di Fermo DEPOSITO.
090404180223     c                   exsr      Chiodo_GEODIS
090406180223      *
090500960528     C                   MOVEL     '7  '         DB2005
090600050909      * ----->
090700050919      *   Imposta la Data e le ore 18:00 fisse. Prima prendeva sempre la data
090800050919      *   apertura/riapertura della giacenza invece,
090900050919      *    questa è la data della distinta di consegna da ARB oppure
091000050919      *    la UDATE se si è aperta giacenza non passando da distinta.
091100050919     C                   movel     gcpDXD        WDATHM
091200050909     C                   move      '1800'        WDATHM
091300050909     C                   movel     WDATHM        DB2380
091400050922      *
091500050922      * Controlla se è stato fatto un evento di giacenza senza aver
091600050922      *  successivamente riaperto Giacenza ... prende questo come Data/Ora
091700050922     C     KGCP_1        setgt     fnEVB04L
091800050922     C     KGCP_1        readpe    fnEVB04L
091900050922     c                   dow       not %EoF(fnEVB04L)
092000050922     c     evbCEV        lookup    GiaC                                   31
092100050922     c                   if        *in31 = *on
092200050922     c                   if        evbDEV > gcpDXD
092300050922     C                   movel     evbDEV        WDATHM
092400050922     C                   move      evbHEV        WDATHM
092500050922     C                   movel     WDATHM        DB2380
092600050922     c                   end
092700050922     c                   leave
092800050922     c                   end
092900050922     C     KGCP_1        readpe    fnEVB04L
093000050922     c                   enddo
093100120830      *
093200050909     C                   movel     '203'         DB2379
093300050922      *
093400980220      *  Scrivo record SD00 e SD05
093500980220     C                   EXSR      WRT0E5
093600980220     C                   GOTO      FIND05
093700970617     C                   END                                                    -- 02 --
093800970617     C                   END                                                    -- 01 --
093900050922
094000050922     c                   end                                                    -- 0B --
094100050922     c                   end                                                    -- 0A --
094200980220      *-------------------------------------------------------------*
094300980220      *  La spedizione ha avuto lasciato avviso
094400980220     C     VACDLA        IFGT      VACDCM
094500960528     C     VACDLA        ANDGT     0
094600980220     C*  Controllo se il codice lasciato avviso è codificato in tabella
094700960528     C                   Z-ADD     1             X
094800960528     C     'AVV'         LOOKUP    C2A(X)                                 32
094900980220      *
095000960528     C     *IN32         IFEQ      '1'
095100980220      *
095200961022     C     §PULAB        IFEQ      'S'
095300960528     C                   MOVEL     CEV(X)        DB9013
095400960612     C                   MOVEL     STS(X)        DB9011
095500961022     C                   ELSE
095600961022     C                   MOVEL     CDV(X)        DB9013
095700961022     C                   MOVEL     STV(X)        DB9011
095800961022     C                   END
095900980220      *
096000960528     C                   MOVEL     '7  '         DB2005
096100050926     C                   movel     VACDLA        WDATHM
096200050926     C                   move      1800          WDATHM
096300050926     C                   movel     WDATHM        DB2380
096400050926     C                   MOVEL     '203'         DB2379
096500050922      *
096600050922      * Controlla se è stato fatto un evento di L.AVV. posteriore
096700050922      *  a quello presente nel vacDLA ... prende questo come Data/Ora
096800050922     C                   Z-ADD     VACAAS        KAAS
096900050922     C                   Z-ADD     VACLNP        KLNP
097000050922     C                   Z-ADD     VACNRS        KNRS
097100050922     C                   Z-ADD     VACNSP        KNSP
097200050922     C     KGCP_1        setgt     fnEVB04L
097300050922     C     KGCP_1        readpe    fnEVB04L
097400050922     c                   dow       not %EoF(fnEVB04L)
097500050926     c     evbCEV        lookup    LAvv                                   31
097600050922     c                   if        *in31 = *on
097700050922     c                   if        evbDEV >= vacDLA
097800050922     C                   movel     evbDEV        WDATHM
097900050922     C                   move      evbHEV        WDATHM
098000050922     C                   movel     WDATHM        DB2380
098100050922     C                   MOVEL     '203'         DB2379
098200050922     c                   end
098300050922     c                   leave
098400050922     c                   end
098500050922     C     KGCP_1        readpe    fnEVB04L
098600050922     c                   enddo
098700050922
098800980220      *  Scrivo record SD00 e SD05
098900980220     C                   EXSR      WRT0E5
099000980220     C                   GOTO      FIND05
099100960528     C                   END
099200970617     C                   END                                                    -- 01 --
099300980220      *-------------------------------------------------------------*
099400081117      *  -> STATUS 20/210 Appuntamento (SUPERMERCATO)
099500081117      * Non è un evento ma una particolarità di consegna
099600081117      *  aggiunto anche per appuntamento (Booking ins)
099700081117     c                   eval      Bolla_variata  = 'N'
099800081117      *
099900140221      *   Solo se NON negato l'INVIO sulla tab.PT(PS)
100000140221     C                   if        Invio_Appuntamento_Supermercato <>'N'
100100081117     c                   if        VACTC1 = 'S' or VACTC2 = 'S' or
100200081117     c                             VACTC1 = 'A' or VACTC2 = 'A'
100300081117     c                   Exsr      STATUS_20_210
100400081117     c                   end
100500140221     c                   end
100600081117      *
100700081117      *-------------------------------------------------------------*
100800980220      *  La spedizione è stata chiusa per: RESO - IDD - CONSEGNA
100900970617     C     VACDCM        IFGT      0                                            -- 01 --
101000970617     C     VACCCA        ANDNE     '7'
101100970617     C     VACCCA        ANDNE     'P'
101200970617     C     BLPDCM        ORGT      0
101300980220      *
101400980220      *---------------------------------*
101500980220      *  RESO
101600970617     C     VACCCA        IFEQ      '2'                                          -- 02 --
101700960712     C     VACCCA        OREQ      '6'
101800980220      *
101900980220      *  Controllo se reso codificato in tab.
102000960528     C                   Z-ADD     1             X
102100980220     C     'RES'         LOOKUP    C2A(X)                                 32      ??? RESO
102200970617     C     *IN32         IFEQ      '1'                                          -- 03 --
102300961022     C     §PULAB        IFEQ      'S'
102400960528     C                   MOVEL     CEV(X)        DB9013
102500960612     C                   MOVEL     STS(X)        DB9011
102600961022     C                   ELSE
102700961022     C                   MOVEL     CDV(X)        DB9013
102800961022     C                   MOVEL     STV(X)        DB9011
102900961022     C                   END
103000960528     C                   MOVEL     '7  '         DB2005
103100960528     C                   MOVEL     VACDCM        WDATHM
103200960528     C                   MOVE      VACHMC        WDATHM           12 0
103300960528     C                   MOVEL     WDATHM        DB2380
103400960528     C                   MOVEL     '203'         DB2379
103500980220      *  Scrivo record SD00 e SD05
103600980220     C                   EXSR      WRT0E5
103700980220     C                   GOTO      FIND05
103800970617     C                   END                                                    -- 03 --
103900980220      *---------------------------------*
104000970617     C                   ELSE                                                   -- 02 --
104100980220      *---------------------------------*
104200980220      *  COLLI MANCANTI - IDD -
104300120830      *
104400970617     C     VACCCA        IFEQ      '5'                                          -- 03 --
104500970617     C     VACCCA        OREQ      '3'
104600970617     C     VACCCA        OREQ      '7'
104700970319     C     WTREVE        OREQ      'S'
104800980220      *
104900980220      *  Controllo se reso codificato in tab.
105000970121     C                   Z-ADD     1             X
105100970121     C     'IDD'         LOOKUP    C2A(X)                                 32     IDD
105200980220      *
105300970617     C     *IN32         IFEQ      '1'                                          -- 04 --
105400970121     C     §PULAB        IFEQ      'S'
105500970121     C                   MOVEL     CEV(X)        DB9013
105600970121     C                   MOVEL     STS(X)        DB9011
105700970121     C                   ELSE
105800970121     C                   MOVEL     CDV(X)        DB9013
105900970121     C                   MOVEL     STV(X)        DB9011
106000970121     C                   END
106100970121     C                   MOVEL     '7  '         DB2005
106200970121     C                   MOVEL     VACDCM        WDATHM
106300970121     C                   MOVE      VACHMC        WDATHM           12 0
106400970121     C                   MOVEL     WDATHM        DB2380
106500970121     C                   MOVEL     '203'         DB2379
106600980220      *  Scrivo record SD00 e SD05
106700980220     C                   EXSR      WRT0E5
106800980220     C                   GOTO      FIND05
106900970617     C                   END                                                    -- 04 --
107000980220      *---------------------------------*
107100970617     C                   ELSE                                                   -- 03 --
107200980319      *---------------------------------*
107300980319      * Se VACCCA='C' --> ho ricevuto dall'arrivo e devo trasmettere
107400980319      *                   al partner un evento di messa in consegna
107500980319     C     VACCCA        IFEQ      'C'                                          -- 04 --
107600040707      *---------------------------------*
107700040707      *  Prima del MIC        -> STATUS 20/210 SUPERMERCATO
107800040707      *
107900040707      * Non è un evento ma una prticolarità di consegna
108000040721      *  aggiunto anche per appuntamento (Booking ins)
108100081111      *    SOLO SE NON E'STAT MANDATA PRIMA:
108200081112     c                   if        Bolla_variata  = 'N'
108300081112      *
108400140221      *   Solo se NON negato l'INVIO sulla tab.PT(PS)
108500140221     C                   if        Invio_Appuntamento_Supermercato <>'N'
108600081111     c                   if        VACTC1 = 'S' or VACTC2 = 'S' or
108700081111     c                             VACTC1 = 'A' or VACTC2 = 'A'
108800081111     c                   Exsr      STATUS_20_210
108900081111     c                   end
109000140221     c                   end
109100081119      *
109200081111     c                   endif
109300040707      *---------------------------------*
109400980319     C                   Z-ADD     1             X
109500980319     C     'MIC'         LOOKUP    C2A(X)                                 32
109600980319      *
109700980319     C     *IN32         IFEQ      '1'                                          -- 05 --
109800980319     C     §PULAB        IFEQ      'S'
109900980319     C                   MOVEL     CEV(X)        DB9013
110000980319     C                   MOVEL     STS(X)        DB9011
110100980319     C                   ELSE
110200980319     C                   MOVEL     CDV(X)        DB9013
110300980319     C                   MOVEL     STV(X)        DB9011
110400980319     C                   END
110500980319     C                   MOVEL     '7  '         DB2005
110600980319     C                   MOVEL     VACDCM        WDATHM
110700980319     C                   MOVE      VACHMC        WDATHM           12 0
110800980319     C                   MOVEL     WDATHM        DB2380
110900980319     C                   MOVEL     '203'         DB2379
111000980319      *  Scrivo record SD00 e SD05
111100980319     C                   EXSR      WRT0E5
111200980319     C                   GOTO      FIND05
111300980319     C                   END                                                    -- 05 --
111400980319     C                   ELSE                                                   -- 04 --
111500980220      *---------------------------------*
111600980220      *  CONSEGNA TOTALE
111700970617     C                   Z-ADD     1             X
111800960528     C     'CON'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
111900161013     c   32              eval      Delivery_STATUS = 'S'
112000980220      *
112100980319     C     *IN32         IFEQ      '1'                                          -- 05 --
112200961022     C     §PULAB        IFEQ      'S'
112300961022     C                   MOVEL     CEV(X)        DB9013
112400961022     C                   MOVEL     STS(X)        DB9011
112500961022     C                   ELSE
112600961022     C                   MOVEL     CDV(X)        DB9013
112700961022     C                   MOVEL     STV(X)        DB9011
112800961022     C                   END
112900960528     C                   MOVEL     '7  '         DB2005
113000960528     C                   MOVEL     VACDCM        WDATHM
113100960528     C                   MOVE      VACHMC        WDATHM           12 0
113200960528     C                   MOVEL     WDATHM        DB2380
113300960528     C                   MOVEL     '203'         DB2379
113400980220      *  Scrivo record SD00 e SD05
113500980220     C                   EXSR      WRT0E5
113600980220     C                   GOTO      FIND05
113700980319     C                   END                                                    -- 05 --
113800980220      *---------------------------------*
113900980319     C                   END                                                    -- 04 --
114000980319     C                   END                                                    -- 03 --
114100970617     C                   END                                                    -- 02 --
114200980220      *---------------------------------*
114300980220      *  La spedizione è stata chiusa per: IDD PARTENZA
114400980220     C                   ELSE
114500980220      *---------------------------------*
114600980220      * Chiusura bolla x IDD in partenza
114700970617     C     VACCCA        IFEQ      '7'                                          -- 02 --
114800970617     C     VACCCA        OREQ      'P'                                          -- 02 --
114900970617     C                   Z-ADD     1             X
115000980204     C                   Z-ADD     VACAAS        KAA2
115100980204     C                   Z-ADD     VACLNP        KLNP
115200980204     C                   Z-ADD     VACNRS        KNRS
115300980204     C                   Z-ADD     VACNSP        KNSP
115400990810     C                   Z-ADD     WOGGI         KDTV
115500980204     C                   TIME                    KORV
115600990810     C     KEVB1         SETGT     FNEVB05L
115700990810     C     KEVB2         READPE    FNEVB05L                               31
115800980220      *
115900980204     C     EVBCEV        LOOKUP    C2A(X)                                 32     IDD
116000980220      *
116100970617     C     *IN32         IFEQ      '1'                                          -- 03 --
116200970617     C     §PULAB        IFEQ      'S'
116300970617     C                   MOVEL     CEV(X)        DB9013
116400970617     C                   MOVEL     STS(X)        DB9011
116500970617     C                   ELSE
116600970617     C                   MOVEL     CDV(X)        DB9013
116700970617     C                   MOVEL     STV(X)        DB9011
116800970617     C                   END
116801180223      *
116802180223      * Per la GEODIS c'è un chiodo al momento sul codice "032"
116803180223      *  x il quale non si deve inviare il 56+265 generico ma un codice
116804180223      *    inventato di Fermo DEPOSITO.
116805180223     c                   exsr      Chiodo_GEODIS
116806180223      *
116900970617     C                   MOVEL     '7  '         DB2005
117000970617     C                   MOVEL     VACDCM        WDATHM
117100970617     C                   MOVE      VACHMC        WDATHM           12 0
117200970617     C                   MOVEL     WDATHM        DB2380
117300970617     C                   MOVEL     '203'         DB2379
117400980220      *  Scrivo record SD00 e SD05
117500980220     C                   EXSR      WRT0E5
117600980220     C                   GOTO      FIND05
117700970617     C                   END                                                    -- 03 --
117800970617     C                   END                                                    -- 02 --
117900980220      *---------------------------------*
118000970617     C                   END
118100980220      *
118200960528     C     FIND05        ENDSR
118300040604      *-------------------------------------------------------------*
118400040604      *? STATUS_20_210      << SUPERMERCATO >>
118500040604      *----------------------------------------------------------------
118600040604     C     STATUS_20_210 Begsr
118700040604      *
118800081112     c                   eval      variata    = 'N'
118900081112      *
119000040607      * Si deve prendere l'ultima variazione del solo tipo record ARBG
119100040607      *  appartenente ad una causale CR, CP, CS, CD
119200040607     C                   Z-ADD     VACAAS        KAAS
119300040607     C                   Z-ADD     VACLNP        KLNP
119400040607     C                   Z-ADD     VACNRS        KNRS
119500040607     C                   Z-ADD     VACNSP        KNSP
119600140221     c                   clear                   salva_data
119700140221     c                   clear                   salva_ora
119800140218      *
119900131014     c     kARF          setgt     fiarbf2c
120000131014     c                   setoff                                       14
120100140218      *
120200131014     c     kARF          readpe    fiarbf2c
120300131014     c                   dow       not %eof(fiarbf2c)
120400040607      *
120500040607      * solo x tipo record FNARBG00 (variazione bolla)
120600080922     c                   if        *in14 = *on
120700080922      *
120800081111      ** con causale particolare
120900140221     c                   if        G_arbCVB ='CR' or G_arbCVB ='CS' or
121000140221     c                             G_arbCVB ='CD'
121100140221      *
121200140221      * Se presente una variazione anche se NON Appuntamento/Supermercato memorizzo
121300140221      *   ...può servire in futuro
121400140221     c                   eval      Bolla_variata  = 'S'
121500140221     ****
121600140221      *  INVECE:
121700140221      *   X dire che si tratti di una bolla VARIATA come APPUNTAMENTO/SUPERMERCATO
121800140221      *    occorre percorrere a ritroso i records di variazione x prendere il primo
121900140221      *    (fra gli ultimi) variato a causa dell'appuntamento o supermercato
122000140221     c                   if        G_arbtc1 <> 'A' and G_arbtc2 <> 'A' and
122100140221     c                             G_arbtc1 <> 'S' and G_arbtc2 <> 'S'
122200140221     ****
122300140221     c                   eval      salva_data = G_ARBdtv
122400140221     c                   eval      salva_ora  = G_ARBorv
122500140221     ****
122600140221     ****************
122700081111      *  Solo se la variazione è avvenuta in giornata viene inviata altrimenti non
122800081111      *   deve essere presa in considerazione:
122900140221     c*********          if        G_arbDTV = wOggi
123000140221     c************       eval      variata        = 'S'
123100140221     c************       leave
123200140221     c*********          endIf
123300140221      *******   SPOSTATE FUORI DAL CICLO
123400140221     ****************
123500140221     ****
123600140221     c                   else
123700140221     ****  solo se aveva preso una data di variazione nella lettura precedente
123800140221     c                   if        salva_data > 0
123900140221     c                   LEAVE
124000140221     c                   end
124100140221     c                   endif
124200140221     ****
124300040607     c                   end
124400040607     c                   end
124500080922      *
124600131014     c                   setoff                                       14
124700131014     c     kARF          readpe    fiarbf2c
124800040607     c                   enddo
124900140221     ****
125000140221     ****   PORTATE QUI Fuori Ciclo Variazioni
125100140221      *  Solo se la variazione è avvenuta in giornata viene inviata altrimenti
125200140221      *   NON deve essere presa in considerazione:
125300140221     c                   if        salva_Data > 0
125400140221      *
125500140221     c                   if        salva_Ora <= vacHMC and salva_Data <= vacDCM
125600140221     c                   eval      variata        = 'S'
125700140221     c                   endIf
125800140221      *
125900140221     c                   endIf
126000140221     ****
126100140221     ****
126200140221     ****
126300140221     ****
126400140221     ****
126500040607      *
126600040607     c                   if        variata = 'S'
126700040607      * data e ora variazione
126800140221     C******             MOVEL     G_arbORV      Wora              4 0
126900140221     C                   MOVEL     salva_ORA     Wora              4 0
127000040607     C                   MOVE      Wora          WDATHM           12 0
127100140221     C******             MOVEL     G_arbDTV      WDATHM
127200140221     C                   MOVEL     salva_Data    WDATHM
127300040607     c                   else
127400040607      * data spedizione
127500040607     C                   MOVEL     vacaas        Wdata             8 0
127600040607     C                   MOVE      vacmgs        Wdata
127700040607     C                   MOVE      '0800'        WDATHM           12 0
127800040607     C                   MOVEL     Wdata         WDATHM
127900040607     c                   end
128000040607      *
128100040607      *  20+210 --> segnala se consegnata a un Supermercato
128200040608     C                   MOVEL     '20 '         DB9011
128300040608     C                   MOVEL     '210'         DB9013
128400040607     C                   MOVEL     '7  '         DB2005
128500040607     C                   MOVEL     WDATHM        DB2380
128600040607     C                   MOVEL     '203'         DB2379
128700040607      *
128800040607      *  Scrivo record SD00 e SD05 prima dei 2 records della consegna
128900040607     C                   EXSR      WRT0E5
129000040607      *
129100040607      * e nel FTX la data di consegna richiesta se presente
129200040607      *  Pulisco dati DS
129300040607     c                   if        vacdcr <> *zero
129400040607     C                   MOVEL     *BLANKS       WDAT
129500040607     C                   CLEAR                   EDSD15
129600040607      *
129700040607      *   Note generiche ='AAI'
129800040607     C                   MOVEL     'AAI'         DD4451
129900040607     c                   select
130000040607     c                   when      vactcr = *blank
130100040607     C                   eval      DD4440 = 'ON ' +
130200040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
130300040607     c                             %editw(vachcr:'  :  ')
130400040607     c                   when      vactcr = 'P'
130500040607     C                   eval      DD4440 = 'BEFORE ' +
130600040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
130700040607     c                             %editw(vachcr:'  :  ')
130800040607     c                   when      vactcr = 'D'
130900040607     C                   eval      DD4440 = 'AFTER ' +
131000040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
131100040607     c                             %editw(vachcr:'  :  ')
131200040607     c                   endsl
131300040607      *  Scrivo record dati note
131400040607     C                   MOVEL     EDSD15        WDAT
131500040607     C                   Z-ADD     §MSNUM        STAPRG
131600040607     C                   MOVEL     'SD15'        STATPR
131700040607     C                   MOVEL     WDAT          STADAT
131800120830      *
131900120830      *  Testata messaggio  FTX-
132000120830     c                   exsr      write_OUTPUT
132100110315      *
132200080422     c                   eval      Righe_Dett = 'S'
132300040607     C                   End
132400040607      *
132500040604     C                   Endsr
132600980220      *-------------------------------------------------------------*
132700980220      *? NUMCMR - Scrivo numero CMR
132800980220      *----------------------------------------------------------------
132900980220     C     NUMCMR        BEGSR
133000980220      *
133100980220     C                   MOVEL     '784'         DB1153
133200980220     C     WTRRDE        IFEQ      'S'
133300980220      *
133400980220     C     §PTCM1        IFEQ      'S'
133500980220     C     §PTNF1        OREQ      'S'
133600980220     C                   Z-ADD     VACAAS        KAAS
133700980220     C                   Z-ADD     VACLNP        KLNP
133800980220     C                   Z-ADD     VACNRS        KNRS
133900980220     C                   Z-ADD     VACNSP        KNSP
134000980220     C                   MOVEL     'TD1154'      KNMC
134100980220     C                   Z-ADD     1             KNRP1
134200980220     C     KRDE          CHAIN     EDRDE01L                           31
134300980220     C  N31              MOVEL     RDEVAL        DB1154
134400980220     C                   END
134500980220      *
134600980220     C                   END
134700980220      *
134800980220     C                   ENDSR
134900980220      *-------------------------------------------------------------*
135000980220      *? WRT0E5 - Scrittura records d00 e d05
135100980220      *----------------------------------------------------------------
135200980220     C     WRT0E5        BEGSR
135300120830      **
135400980220      *  Scrivo tipo record SD00
135500980220     C                   ADD       1             WPROG
135600980220     C                   MOVEL     WPROG         DA1490
135700980220     C                   MOVEL     '9999'        DA1312
135800980220     C                   CLEAR                   WDAT
135900980220     C                   MOVEL     EDSD00        WDAT
136000980220     C                   Z-ADD     §MSNUM        STAPRG
136100980220     C                   MOVEL     'SD00'        STATPR
136200980220     C                   MOVEL     WDAT          STADAT
136300120830      *
136400120830      *  Testata messaggio  CNI-
136500120830     c                   exsr      write_OUTPUT
136600110315      *
136700980220      *  Scrivo tipo record SD05
136800980220     C                   MOVEL     EDSD05        WDAT
136900980220     C                   Z-ADD     §MSNUM        STAPRG
137000980220     C                   MOVEL     'SD05'        STATPR
137100980220     C                   MOVEL     WDAT          STADAT
137200120830      *
137300120830      *  Testata messaggio  STS-RFF-DTM
137400120830     c                   exsr      write_OUTPUT
137500110316      *
137600980220     C                   MOVEL     'S'           WRTEVE            1
137700080422     c                   eval      Righe_Dett = 'S'
137800980220      *
137900980220     C                   ENDSR
138000980220      *-------------------------------------------------------------*
138100980220      *? WRSD10 - Scrittura record partner - dettaglio
138200980220      *----------------------------------------------------------------
138300980220     C     WRSD10        BEGSR
138400101028      *
138500101028      *  solo chi vuole ricevere la firma del firmatario
138600101028     C                   IF        §PUfirma <>'N'
138700980220      *
138800960712     C                   MOVEL     *BLANKS       WDAT
138900960528     C                   CLEAR                   EDSD10
139000101028      *
139100101028      *  Per Schneider questo segmento NON DEVE essere inviato
139200101028      *
139300980220      *  Controllo se esiste firmatario
139400960528     C                   MOVEL     '1'           KTRC
139500060213     C     Kar4          CHAIN     fiar401L                           31
139600960528     C     *IN31         IFEQ      '0'
139700960528     C                   MOVEL     'AP '         DC3035
139800060213     C                   MOVEL     ar4NOT        DC3036
139900980220      *  Scrivo record dati firmatario
140000960705     C                   MOVEL     EDSD10        WDAT
140100960729     C                   Z-ADD     §MSNUM        STAPRG
140200960705     C                   MOVEL     'SD10'        STATPR
140300960705     C                   MOVEL     WDAT          STADAT
140400120830      *
140500120830      *  Testata messaggio  NAD-
140600120830     c                   exsr      write_OUTPUT
140700110316      *
140800080422     c                   eval      Righe_Dett = 'S'
140900960528     C                   END
141000980220      *
141100101028     c                   end
141200161013      *-------
141300161013      *  solo chi vuole ricevere la Ragione Sociale del Destinatario
141400161013     C                   IF        Invio_NAD_CN  = 'S'
141500161013      *  solo sulla delivery oppure sempre
141600161013     C                   IF        Delivery_STATUS = 'S' and
141700161013     c                             Invio_NAD_CN_Solo_in_Delivery = 'S'
141800161013     c                               or Invio_NAD_CN_Solo_in_Delivery = *blank
141900161013      *--
142000161013     C                   MOVEL     *BLANKS       WDAT
142100161013     C                   CLEAR                   EDSD10
142200161013      *  fatto per Agility
142300161013     C                   MOVEL     'CN '         DC3035
142400161013     C                   MOVEL     blpRSD        DC3036
142500161013      *  Scrivo record dati firmatario
142600161013     C                   MOVEL     EDSD10        WDAT
142700161013     C                   Z-ADD     §MSNUM        STAPRG
142800161013     C                   MOVEL     'SD10'        STATPR
142900161013     C                   MOVEL     WDAT          STADAT
143000161013      *
143100161013      *   Dettaglio  NAD-CN
143200161013     c                   exsr      write_OUTPUT
143300161013      *
143400161013     c                   eval      Righe_Dett = 'S'
143500161013     c                   end
143600161013     c                   end
143700161013      *
143800960528     C                   ENDSR
143900980220      *-------------------------------------------------------------*
144000980220      *? WRSD15 - Scrittura record partner - dettaglio
144100980220      *----------------------------------------------------------------
144200960528     C     WRSD15        BEGSR
144300980220      *
144400980220      *  Pulisco dati DS
144500960712     C                   MOVEL     *BLANKS       WDAT
144600960528     C                   CLEAR                   EDSD15
144700980220      *
144800980220      *  Controllo se ci sono delle note di consegna
144900980220     C     VACDCM        IFNE      0                                            -- 01 --
145000980204     C                   MOVEL     *BLANKS       RDEVAL
145100980220      *
145200980220     C     WTRRDE        IFEQ      'S'                                          -- 02 --
145300980204     C                   Z-ADD     VACAAS        KAAS
145400980204     C                   Z-ADD     VACLNP        KLNP
145500980204     C                   Z-ADD     VACNRS        KNRS
145600980204     C                   Z-ADD     VACNSP        KNSP
145700980204     C                   MOVEL     'DD4440'      KNMC
145800980204     C                   Z-ADD     1             KNRP1
145900980204     C     KRDE          CHAIN     EDRDE01L                           31
146000980220      *
146100980220     C     *IN31         IFEQ      '0'                                          -- 03 --
146200980204     C                   MOVEL     'AAI'         DD4451
146300170330      **
146400170330      **  bisogna togliere tutti i caratteri speciali
146500170330     c     '''':' '      xlate     rdeval        rdeval
146600170330     c     '?':' '       xlate     rdeval        rdeval
146700170330     c     '+':' '       xlate     rdeval        rdeval
146800170330     c     ':':' '       xlate     rdeval        rdeval
146900170330      **
147000980204     C                   MOVEL     RDEVAL        DD4440
147100980204     C                   DELETE    EDRDE000
147200980220      * controllo se c'è proseguimento
147300980204     C                   Z-ADD     2             KNRP1
147400980204     C     KRDE          CHAIN     EDRDE01L                           31
147500980220      *
147600980220     C     *IN31         IFEQ      '0'                                          -- 04 --
147700980204     C                   MOVE      'AAI'         DD4451
147800170330      **
147900170330      **  bisogna togliere tutti i caratteri speciali
148000170330     c     '''':' '      xlate     rdeval        rdeval
148100170330     c     '?':' '       xlate     rdeval        rdeval
148200170330     c     '+':' '       xlate     rdeval        rdeval
148300170330     c     ':':' '       xlate     rdeval        rdeval
148400170330      **
148500980204     C                   MOVE      RDEVAL        DD4440
148600980204     C                   DELETE    EDRDE000
148700980220     C                   END                                                    -- 04 --
148800980220      *
148900980220      *  Scrivo record dati note
149000980204     C                   MOVEL     EDSD15        WDAT
149100980204     C                   Z-ADD     §MSNUM        STAPRG
149200980204     C                   MOVEL     'SD15'        STATPR
149300980204     C                   MOVEL     WDAT          STADAT
149400120830      *
149500120830      *  Testata messaggio  FTX-
149600120830     c                   exsr      write_OUTPUT
149700110315      *
149800080422     c                   eval      Righe_Dett = 'S'
149900980220     C                   END                                                    -- 03 --
150000980220     C                   END                                                    -- 02 --
150100980220      *-------------------------------
150200980220     C                   ELSE                                                   -- 01 --
150300980220      *-------------------------------
150400980220      *
150500980220      *  Se  è una giacenza cerco le note giacenza
150600961204     C                   Z-ADD     GCPAGC        KAGC
150700961204     C                   Z-ADD     GCPFGC        KFGC
150800961204     C                   Z-ADD     GCPNGC        KNGC
150900961204     C                   Z-ADD     10            KFAS
151000050309     C     KGNP          CHAIN     tignp01L                           31
151100980220     C     *IN31         IFEQ      '0'                                          -- 02 --
151200961204     C                   MOVEL     'AAI'         DD4451
151300961204     C                   MOVEL     GNPDMC        DD4440
151400961204     C*  Scivo record dati note giacenza
151500961204     C                   MOVEL     EDSD15        WDAT
151600961204     C                   Z-ADD     §MSNUM        STAPRG
151700961204     C                   MOVEL     'SD15'        STATPR
151800961204     C                   MOVEL     WDAT          STADAT
151900120830      *
152000120830      *  Testata messaggio  FTX-
152100120830     c                   exsr      write_OUTPUT
152200110316      *
152300080422     c                   eval      Righe_Dett = 'S'
152400980220     C                   END                                                    -- 02 --
152500980220     C                   END                                                    -- 01 --
152600961204     C*
152700960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
152800980220     C     WSD15         IFEQ      'S'                                          -- 01 --
152900960821     C                   MOVEL     *BLANKS       WDAT
153000960821     C                   CLEAR                   EDSD15
153100980220     C                   END                                                    -- 01 --
153200980220      *
153300960528     C                   ENDSR
153400960528     C*-------------------------------------------------------------*
153500960528     C*? WRSD20 - Scrittura record partner - dettaglio
153600980219      *----------------------------------------------------------------
153700960528     C     WRSD20        BEGSR
153800980219      *
153900980219      *  Pulisco dati DS
154000960912     C                   MOVEA     *BLANKS       SG1
154100960912     C                   MOVEA     *BLANKS       SG2
154200970318     C                   MOVEA     *BLANKS       SG3
154300970619     C                   MOVEA     *BLANKS       SG4
154400960912     C                   MOVEL     *BLANKS       WDAT
154500960528     C                   CLEAR                   EDSD20
154600960528     C                   MOVEL     '99999'       DE1496
154700960729     C                   MOVE      VACNCL        DE7224
154800980220      *
154900990604      *  Se è stata aperta C.A. elaboro a parte
155000990604     C     WELACA        IFEQ      'S'
155100990604     C                   EXSR      COLCA
155200990604     C                   GOTO      ED20
155300990604     C                   ENDIF
155400990604      *
155500990604      *  Se sto effettuando la consegna totale di una bolla con codice
155600980219      *  consegna anomala = '5' su cui è stata effettuata una consegna
155700980219      *  parziale risfleggo eventuali colli trasmessi e chiusi con IDD
155800980220      *
155900980220     C     BLPCCA        IFEQ      '5'
156000970319     C     WTREVE        OREQ      'S'
156100980220      *
156200970319     C     BLPDCM        IFGT      0
156300970318     C     BLPDCP        ANDGT     0
156400970318     C                   EXSR      LIBIDD
156500970318     C                   END
156600980220      *
156700970319     C                   END
156800980220      *
156900980220      *  Se tabellato devo sempre inviare il dettaglio segnacolli
157000980220      *  Carico tutto
157100970304     C     §PTDET        IFEQ      'S'                                          --- 02 ---
157200960912     C                   EXSR      CARCOL
157300980220     C                   EXSR      WRTCOL
157400960912     C                   END
157500980220      *
157600980223      *  Se tabellato devo inviare dettaglio solo per consegne parziali
157700980223      *  Carico non consegnati
157800970304     C     §PTDET        IFEQ      'N'                                          --- 02 ---
157900970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
158000980223      *
158100980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
158200980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
158300970303     C     BLPCCA        OREQ      '5'
158400970617     C     BLPCCA        OREQ      '3'
158500970619     C     VACCCA        OREQ      '7'
158600970619     C     VACCCA        OREQ      'P'
158700970319     C     WTREVE        OREQ      'S'
158800960912     C                   EXSR      CARCOL
158900980220     C                   EXSR      WRTCOL
159000960912     C                   ELSE
159100980223      *
159200980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
159300970304     C     BLPDCM        IFNE      0
159400970304     C                   EXSR      TUTTI
159500970304     C                   END
159600960912     C                   EXSR      WRT20
159700970304     C                   END                                                    --- 03 ---
159800970304     C                   END                                                    --- 02 ---
159900980223      *
160000980223      *  Se previsto da tabella che non devo mai passare il
160100980223      *  numero dei colli scrivo D20
160200970304     C     §PTDET        IFEQ      'M'                                          --- 02 ---
160300980223      *  Per consegne parziali o IDD loop su dettaglio segnacolli
160400970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
160500980223      *
160600980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
160700980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
160800970304     C     BLPCCA        OREQ      '5'
160900970617     C     BLPCCA        OREQ      '3'
161000970619     C     VACCCA        OREQ      '7'
161100970619     C     VACCCA        OREQ      'P'
161200970319     C     WTREVE        OREQ      'S'
161300970304     C                   EXSR      REDBLT
161400970304     C                   ELSE
161500980223      *
161600980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
161700970304     C     BLPDCM        IFEQ      0
161800970304     C                   EXSR      TUTTI
161900970304     C                   END
162000970304     C                   EXSR      WRT20
162100970304     C                   END                                                    --- 03 ---
162200970304     C                   END                                                    --- 02 ---
162300980223      *
162400990604     C     ED20          ENDSR
162500980220      *----------------------------------------------------------------
162600980220      *? LIBIDD - Controllo se devo sfleggare colli con IDD
162700980220      *----------------------------------------------------------------
162800970318     C     LIBIDD        BEGSR
162900980219      *
163000980219      *  Lettura BLT e caricamento colli
163100970318     C     KBLT          CHAIN     FNBLT01L                           31
163200980219      *  rislego i colli da trasmettete al cliente
163300980220     C     *IN31         DOWEQ     '0'
163400980220      *
163500970318     C     BLTFTR        IFEQ      'C'
163600970318     C     BLTDCM        ANDGT     0
163700970318     C     BLTFL1        ANDEQ     '5'
163800970318     C                   MOVEL     ' '           BLTFTR
163900970318     C                   EXCEPT    AGGTRA
164000970318     C                   END
164100980219      * Lettura successiva
164200970318     C     KBLT          READE     FNBLT01L                               31
164300970318     C                   END
164400980219      *
164500970318     C                   ENDSR
164600980219      *----------------------------------------------------------------
164700980219      *? CARCOL - Routine caricamento colli da dettag.
164800980219      *----------------------------------------------------------------
164900960912     C     CARCOL        BEGSR
165000980219      *
165100050413      *  ---------------------
165200050413      *  Legenda dei contatori:   Y1 = Colli Consegnati
165300050413      *  ----------------------   Y2 = Colli Non Consegnati
165400050413      *                           Y3 = Colli Chiusi con IDD
165500050413      *                           Y4 = Colli Negativi con IDD
165600050413      *
165700960912     C                   Z-ADD     0             Y1                5 0
165800960912     C                   Z-ADD     0             Y2                5 0
165900970318     C                   Z-ADD     0             Y3                5 0
166000970619     C                   Z-ADD     0             Y4                5 0
166100980219      *
166200980220      *  Lettura BLT e caricamento colli nelle schiere
166300960912     C     KBLT          CHAIN     FNBLT01L                           31
166400980223      *
166500980220     C     *IN31         DOWEQ     '0'                                          -- 01 --
166600980219      *
166700980219      *  Considero solo i colli non trasmessi al cliente
166800980220     C     BLTFTR        IFNE      'C'                                          -- 02 --
166900980220      *
167000980220      *-------------------------------------------------
167100980219      *  Controllo se devo caricare colli consegnati
167200960912     C     BLTDCM        IFGT      0
167300970303     C     BLTFL1        ANDEQ     ' '
167400970303     C     §PTDET        ANDEQ     'S'
167500970617     C     BLPCCA        ANDNE     '3'
167600980219      *
167700980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
167800980219     C     §PUBS1        IFEQ      'E'
167900980219     C                   EXSR      CARSGE                                       EUROEXPRESS
168000980219     C                   ELSE
168100980219     C                   EXSR      CARSGN                                       BARTOLINI
168200980219     C                   END
168300980219      *
168400980219     C     WSGN          IFNE      *BLANKS
168500960912     C                   ADD       1             Y1
168600960912     C                   MOVEL     WSGN          SG1(Y1)
168700960912     C                   END
168800980219     C                   END
168900980219      *
169000980220      *-------------------------------------------------
169100980219      *  Controllo se devo caricare colli non consegnati
169200960912     C     BLTDCM        IFEQ      0
169300970304     C     §PTDET        ANDNE     'M'
169400970619     C     VACCCA        ANDNE     '7'
169500970304     C     BLTFL1        OREQ      '5'
169600960913     C     §PTDET        ANDNE     'M'
169700970617     C     BLPDCM        ANDGT     0
169800970617     C     BLTFL1        OREQ      '7'
169900970617     C     §PTDET        ANDNE     'M'
170000970617     C     BLTDCM        ANDGT     0
170100970617     C     VACCCA        ANDNE     'P'
170200970617     C     BLPCCA        OREQ      '3'
170300970617     C     §PTDET        ANDNE     'M'
170400970617     C     BLPDCM        ANDGT     0
170500980219      *
170600980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
170700980219     C     §PUBS1        IFEQ      'E'
170800980219     C                   EXSR      CARSGE                                       EUROEXPRESS
170900980219     C                   ELSE
171000980219     C                   EXSR      CARSGN                                       BARTOLINI
171100980219     C                   END
171200980219      *
171300980219     C     WSGN          IFNE      *BLANKS
171400960912     C                   ADD       1             Y2
171500960912     C                   MOVEL     WSGN          SG2(Y2)
171600960912     C                   END
171700980219     C                   END
171800980220      *
171900980220      *-------------------------------------------------
172000980220      *  Controllo se devo caricare colli chiusi con IDD x evento PID
172100970318     C     BLTFL1        IFEQ      '5'
172200970318     C     §PTDET        ANDNE     'M'
172300970318     C     BLPDCM        ANDEQ     0
172400970617     C     BLTFL1        OREQ      '7'
172500970617     C     §PTDET        ANDNE     'M'
172600970617     C     VACCCA        ANDEQ     'P'
172700980220      *
172800980220      *  Se ha segnacollato il cliente carico in schiera i colli cons.
172900980219     C     §PUBS1        IFEQ      'E'
173000980219     C                   EXSR      CARSGE                                       EUROEXPRESS
173100980219     C                   ELSE
173200980219     C                   EXSR      CARSGN                                       BARTOLINI
173300980219     C                   END
173400980219      *
173500980219     C     WSGN          IFNE      *BLANKS
173600970318     C                   ADD       1             Y3
173700970318     C                   MOVEL     WSGN          SG3(Y3)
173800970318     C                   END
173900980220      *
174000980219     C                   END
174100980220      *-------------------------------------------------
174200980220      *  Se devo inviare solo colli negativi ed ho IDD li carico in
174300980220      *  SG4 (nel caso non abbia ancora attivato flag FL1)
174400970619     C     §PTDET        IFEQ      'N'
174500970619     C     BLTDCM        ANDNE     0
174600970619     C     BLTFL1        ANDEQ     ' '
174700980220      *
174800970619     C     BLPCCA        IFEQ      '3'
174900970619     C     BLPCCA        OREQ      '5'
175000980220      *
175100970619     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
175200980219     C     §PUBS1        IFEQ      'E'
175300980219     C                   EXSR      CARSGE                                       EUROEXPRESS
175400980219     C                   ELSE
175500980219     C                   EXSR      CARSGN                                       BARTOLINI
175600980219     C                   END
175700980219      *
175800980219     C     WSGN          IFNE      *BLANKS
175900970619     C                   ADD       1             Y4
176000970619     C                   MOVEL     WSGN          SG4(Y4)
176100970619     C                   END
176200980219      *
176300980219     C                   END
176400970620     C                   END
176500980220      *
176600980220     C                   END                                                    -- 02 --
176700980220      *
176800050331      * Se è sempre richiesto il dettaglio Segnacolli sul Partner/Cliente ed
176900050331      * è una spedizione di RESO:
177000050331      * (poichè lo status di RESO viene inviata 2 volte da noi con 2 date differenti:
177100050331      *   la prima volta con la data della bolla di reso generata, la seconda con la
177200050331      *    data di consegna della bolla di reso. Vedi FNVAC00T)
177300050331      * ALLORA SOLO IN QUESTO CASO non si fleggano i segnacolli come trasmessi.
177400050331      *  in maniera da poterli sempre ritrasmettere al Partner/Cliente.
177500050331      * (Caso ARVATO che ragiona esclusivamente sui segnacolli e non sul rif.Sped.)
177600050331     C     §PTDET        ifEQ      'S'
177700050331     C     VACCCA        IFEQ      '2'
177800050331     C     VACCCA        OREQ      '6'
177900050331     c                   goto      salta_UPD
178000050331     c                   end
178100050331     c                   end
178200050331      *
178300980220      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
178400970304     C     BLTDCM        IFNE      0
178500970303     C                   MOVEL     'C'           BLTFTR
178600970303     C                   EXCEPT    AGGTRA
178700970304     C                   END
178800050331      *
178900050331     c     salta_UPD     tag
179000980220      *
179100980220      * Lettura successiva
179200960912     C     KBLT          READE     FNBLT01L                               31
179300980220     C                   END
179400980220      *
179500980220     C                   ENDSR
179600990604      *----------------------------------------------------------------
179700990604      *? COLCA - Routine caricamento colli da C.A.
179800990604      *----------------------------------------------------------------
179900990604     C     COLCA         BEGSR
180000990604      *
180100990604     C                   Z-ADD     *ZEROS        Y4                5 0
180200990604     C                   Z-ADD     *ZEROS        WCOLLI            5 0
180300040715      *
180400040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
180500040715      * SIA COME MAMMA CHE COME FIGLIA
180600040715     c                   clear                   fidn12ds
180700040715     c                   eval      i12aas = kAAS
180800040715     c                   eval      i12lnp = kLNP
180900040715     c                   eval      i12nrs = kNRS
181000040715     c                   eval      i12nsp = kNSP
181100040715     c                   eval      i12fel = 'E'
181200040715     c                   eval      i12fan = 'E'
181300040715     c                   eval      i12fch = 'E'
181400040715     c                   eval      i12fpt = 'E'
181500040715     c                   call      'FIDN12R'
181600040715     c                   parm                    fidn12ds
181700040715     c                   z-add     *zeros        ii                2 0
181800040715      **
181900040715      * se trovata almeno una CA
182000040715     c                   if        o12nca > 0
182100040715     c                   dow       ii <  o12nca
182200040715      **
182300040715     c                   add       1             ii
182400040715     c                   movea     skkey(ii)     dskey
182500040715     C                   z-add     dsaac         kaac
182600040715     C                   z-add     dsfil         kfil
182700040715     C                   z-add     dsnca         knca
182800040715     c     kdct          chain     FNDCT000
182900040715     c                   If        %found(fndct01l)
183000040715     C                   MOVEL     DCTFLO        DDCT01
183100040715     C     §DCTtrpt      ifeq      *BLANKS
183200990607      *
183300990607      *  Flaggo C.A. come trasmessa
183400020412     C                   MOVEL     'S'           §DCTtrpt
183500990607     C                   MOVEL     DDCT01        DCTFLO
183600990607     C                   EXCEPT    AGGDCT
183700990607      *
183800990604      *  Incremento numero colli
183900990604     C                   ADD       DCTNCN        WCOLLI
184000990604      *
184100990604      *  Controllo se devo caricare i segnacolli
184200990604     C     §PTDET        IFNE      'M'                                          -- 03 --
184300990604      *
184400990604      *  Leggo segnacolli C.A.: non annullati, e non chiusi
184500990604     C     KDCD          CHAIN     FNDCD000                           32
184600990607     C     *IN32         DOWEQ     '0'                                          -- 04 --
184700990604     C     DCDATB        IFEQ      *BLANKS                                      -- 05 --
184800990604     C     DCDDCH        ANDEQ     *ZEROS
184900990604      *
185000990604     C     §PUBS1        IFEQ      'E'
185100990604     C                   EXSR      CARSGE                                       EUROEXPRESS
185200990604     C                   ELSE
185300990604     C                   EXSR      CARSGN                                       BARTOLINI
185400990604     C                   ENDIF
185500990604      *
185600990604     C     WSGN          IFNE      *BLANKS
185700990604     C                   ADD       1             Y4
185800990604     C                   MOVEL     WSGN          SG4(Y4)
185900990604     C                   ENDIF
186000990604      *
186100990604     C                   ENDIF                                                  -- 05 --
186200990607     C     KDCD          READE     FNDCD000                               32
186300990604     C                   ENDDO                                                  -- 04 --
186400040715      *
186500040715     C                   ENDIF                                                  -- 03 --
186600040715      *****
186700040715     C                   endif
186800040715     C                   endif
186900040715      *
187000040715     c                   enddo
187100040715     c                   endif
187200040715      *
187300990607      * SCRIVO FLAT FILE
187400990607     C     §PTDET        IFEQ      'M'                                          -- 01 --
187500990607     C     Y4            OREQ      *ZEROS
187600990607     C                   Z-ADD     WCOLLI        DE7224
187700990607     C                   EXSR      WRT20
187800990607     C                   ELSE
187900990607      *
188000990607     C     Y4            IFNE      *ZEROS                                       -- 02 --
188100050427      *  Nel GID imposta il numero dei Colli Danneggiati
188200050427     C                   Z-ADD     y4            DE7224
188300990607     C                   Z-ADD     *ZEROS        WW
188400990607      *
188500990607     C                   DO        Y4            WE                             -- 03 --
188600990607      *  Scrivo ogni §PUEL2 elementi
188700990607     C     WW            IFEQ      §PUEL2                                       -- 04 --
188800990607     C                   MOVEL     '24'          DE4233
188900990607     C                   EXSR      WRT20
189000990607     C                   MOVEL     *BLANKS       EDSD20
189100990607     C                   Z-ADD     *ZEROS        WW
189200990607     C                   ENDIF                                                  -- 04 --
189300990607     C                   ADD       1             WW
189400990607     C                   MOVEL     SG4(WE)       D20(WW)
189500990607     C                   ENDDO                                                  -- 03 --
189600990607      *
189700990607      *  Se ci sono ancora dei colli scrittura nuovo record
189800990607     C     WW            IFNE      *ZEROS                                       -- 03 --
189900990607     C                   MOVEL     '24'          DE4233
190000990607     C                   EXSR      WRT20
190100990607     C                   MOVEL     *BLANKS       EDSD20
190200990607     C                   ENDIF                                                  -- 03 --
190300990607      *
190400990607     C                   ENDIF                                                  -- 02 --
190500990607     C                   ENDIF                                                  -- 01 --
190600990607      *
190700990604     C                   ENDSR
190800980220      *----------------------------------------------------------------
190900980220      *? WRTCOL - Routine scrittura colli
191000980220      *----------------------------------------------------------------
191100980220     C     WRTCOL        BEGSR
191200980220      *
191300980220      *  Scrivo i colli non consegnati
191400980220     C     Y2            IFGT      0
191500960912     C                   MOVE      Y2            DE7224
191600960912     C                   Z-ADD     0             WW                3 0
191700980220      *
191800980220     C                   DO        Y2            WE                3 0
191900980220      *
192000990607      *  Scrivo ogni §PUEL2 elementi
192100990607     C     WW            IFEQ      §PUEL2
192200990607     C                   MOVEL     '24'          DE4233
192300990607     C                   EXSR      WRT20
192400990607     C                   MOVEL     *BLANKS       EDSD20
192500990607     C                   Z-ADD     0             WW
192600990607     C                   ENDIF
192700990607     C                   ADD       1             WW
192800990607     C                   MOVEL     SG2(WE)       D20(WW)
192900990607     C                   ENDDO
193000990607      *
193100990607      *  Se ci sono ancora dei colli scrittura nuovo record
193200990607     C     WW            IFNE      *ZEROS
193300990607     C                   MOVEL     '24'          DE4233
193400990607     C                   EXSR      WRT20
193500990607     C                   MOVEL     *BLANKS       EDSD20
193600990607     C                   ENDIF
193700990607      *
193800990607     C                   ENDIF
193900980220      *
194000980220      *  Scrivo i colli consegnati
194100980220     C     Y1            IFGT      0                                            -- 01 --
194200980223      *
194300980220      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
194400980220      *  (conse. parziale --> ci sono colli non consegnati)
194500980220      *  (pratica IDD --> ci sono colli persi e colli consegnati)
194600980220     C     Y2            IFGT      0                                            -- 02 --
194700081216     C                   ADD       1             WPROG
194800960912     C                   MOVEL     WPROG         DA1490
194900960912     C                   MOVEL     '9999'        DA1312
195000960912     C                   CLEAR                   WDAT
195100960912     C                   MOVEL     EDSD00        WDAT
195200960912     C                   Z-ADD     §MSNUM        STAPRG
195300960912     C                   MOVEL     'SD00'        STATPR
195400960912     C                   MOVEL     WDAT          STADAT
195500120830      *
195600120830      *  Testata messaggio  GID-PCI
195700120830     c                   exsr      write_OUTPUT
195800110315      *
195900080422     c                   eval      Righe_Dett = 'S'
196000980223      *
196100980220      *  Consegna !!!!!!!
196200960912     C                   Z-ADD     1             X
196300980220     C     'CPA'         LOOKUP    C2A(X)                                 32       ?? CONSE.TOT
196400980220      *
196500980220     C     *IN32         IFEQ      '1'                                          -- 03 --
196600980220     C     §PULAB        IFEQ      'S'                                          -- 04 --
196700961022     C                   MOVEL     CEV(X)        DB9013
196800961022     C                   MOVEL     STS(X)        DB9011
196900980220     C                   ELSE                                                   -- 04 --
197000961022     C                   MOVEL     CDV(X)        DB9013
197100961022     C                   MOVEL     STV(X)        DB9011
197200980220     C                   END                                                    -- 04 --
197300960912     C                   MOVEL     '7  '         DB2005
197400960913     C                   MOVEL     BLPDCP        WDATHM
197500960912     C                   MOVE      VACHMC        WDATHM           12 0
197600960912     C                   MOVEL     WDATHM        DB2380
197700960912     C                   MOVEL     '203'         DB2379
197800980220     C                   END                                                    -- 03 --
197900980223      *
198000980223      *  Scrivo record evento CONSEGNA
198100960912     C                   MOVEL     EDSD05        WDAT
198200960912     C                   Z-ADD     §MSNUM        STAPRG
198300960912     C                   MOVEL     'SD05'        STATPR
198400960912     C                   MOVEL     WDAT          STADAT
198500120830      *
198600120830      *  Testata messaggio  STS-RFF-DTM
198700120830     c                   exsr      write_OUTPUT
198800110315      *
198900080422     c                   eval      Righe_Dett = 'S'
199000980223      *
199100980223      *  Pulisco SD20
199200960913     C                   CLEAR                   EDSD20
199300960913     C                   MOVEL     '99999'       DE1496
199400980220     C                   END                                                    -- 02 --
199500980223      *
199600980223      *  Scrivo D20 da SG1
199700960912     C                   MOVE      Y1            DE7224
199800960912     C                   Z-ADD     0             WW                3 0
199900980220     C                   DO        Y1            WE                3 0          -- 02 --
200000980223      *
200100990607      *  Scrivo ogni §PUEL2 elementi
200200990607     C     WW            IFEQ      §PUEL2
200300960913     C                   MOVEL     '24'          DE4233
200400960912     C                   EXSR      WRT20
200500990607     C                   MOVEL     *BLANKS       EDSD20
200600990607     C                   Z-ADD     *ZEROS        WW
200700990607     C                   ENDIF
200800990607     C                   ADD       1             WW
200900990607     C                   MOVEL     SG1(WE)       D20(WW)
201000990607     C                   ENDDO                                                  -- 02 --
201100980223      *
201200990607      *  Se ci sono ancora dei colli scrittura nuovo record
201300990607     C     WW            IFNE      *ZEROS                                       -- 02 --
201400990607     C                   MOVEL     '24'          DE4233
201500990607     C                   EXSR      WRT20
201600960912     C                   MOVEL     *BLANKS       EDSD20
201700990607     C                   ENDIF                                                  -- 02 --
201800980223      *
201900980220     C                   ELSE                                                   -- 01 --
202000980223      *
202100980223      *  Scrivo D20 da SG1
202200970619     C                   Z-ADD     Y1            DE7224
202300970619     C                   Z-ADD     0             WW                3 0
202400980220     C                   DO        Y1            WE                3 0          -- 02 --
202500990607      *
202600990607      *  Scrivo ogni §PUEL2 elementi
202700990607     C     WW            IFEQ      §PUEL2
202800990607     C                   MOVEL     '24'          DE4233
202900990607     C                   EXSR      WRT20
203000990607     C                   MOVEL     *BLANKS       EDSD20
203100990607     C                   Z-ADD     *ZEROS        WW
203200990607     C                   ENDIF
203300990607     C                   ADD       1             WW
203400990607     C                   MOVEL     SG1(WE)       D20(WW)
203500990607     C                   ENDDO                                                  -- 02 --
203600990607      *
203700990607      *  Se ci sono ancora dei colli scrittura nuovo record
203800990607     C     WW            IFNE      *ZEROS                                       -- 02 --
203900990607     C                   MOVEL     '24'          DE4233
204000990607     C                   EXSR      WRT20
204100990607     C                   MOVEL     *BLANKS       EDSD20
204200990607     C                   ENDIF                                                  -- 02 --
204300980223      *
204400980220     C                   END                                                    -- 01 --
204500980223      *
204600980223      *  Controllo se devo scrivere colli chiusi x IDD parziale
204700980220     C     Y3            IFGT      0                                            -- 01 --
204800980223      *
204900980223      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
205000980223      *  (conse.parziale --> ci sono colli non consegnati)
205100980223      *  (pratica IDD --> ci sono colli persi e colli consegnati)
205200980220     C     Y2            IFGT      0                                            -- 02 --
205300081216     C                   ADD       1             WPROG
205400970318     C                   MOVEL     WPROG         DA1490
205500970318     C                   MOVEL     '9999'        DA1312
205600970318     C                   CLEAR                   WDAT
205700970318     C                   MOVEL     EDSD00        WDAT
205800970318     C                   Z-ADD     §MSNUM        STAPRG
205900970318     C                   MOVEL     'SD00'        STATPR
206000970318     C                   MOVEL     WDAT          STADAT
206100120830      *
206200120830      *  Testata messaggio  GID-PCI
206300120830     c                   exsr      write_OUTPUT
206400110315      *
206500080422     c                   eval      Righe_Dett = 'S'
206600980223      *
206700980223      *  Consegna !!!!!!!
206800970318     C                   Z-ADD     1             X
206900970619     C     VACCCA        IFEQ      '7'
207000970617     C     'MAN'         LOOKUP    C2A(X)                                 32
207100990607     C                   ELSE
207200970617     C     'PID'         LOOKUP    C2A(X)                                 32
207300990607     C                   END
207400980220      *
207500990607     C     *IN32         IFEQ      '1'                                          -- 03 --
207600990607     C     §PULAB        IFEQ      'S'
207700970318     C                   MOVEL     CEV(X)        DB9013
207800970318     C                   MOVEL     STS(X)        DB9011
207900990607     C                   ELSE
208000970318     C                   MOVEL     CDV(X)        DB9013
208100970318     C                   MOVEL     STV(X)        DB9011
208200990607     C                   END
208300970318     C                   MOVEL     '7  '         DB2005
208400970318     C                   MOVEL     BLPDCP        WDATHM
208500970318     C                   MOVE      VACHMC        WDATHM           12 0
208600970318     C                   MOVEL     WDATHM        DB2380
208700970318     C                   MOVEL     '203'         DB2379
208800990607     C                   END                                                    -- 03 --
208900980223      *
209000980223      *  Scivo record evento CONSEGNA
209100970318     C                   MOVEL     EDSD05        WDAT
209200970318     C                   Z-ADD     §MSNUM        STAPRG
209300970318     C                   MOVEL     'SD05'        STATPR
209400970318     C                   MOVEL     WDAT          STADAT
209500120830      *
209600120830      *  Testata messaggio  STS-RFF-DTM
209700120830     c                   exsr      write_OUTPUT
209800110315      *
209900080422     c                   eval      Righe_Dett = 'S'
210000980223      *
210100980223      *  Pulisco SD20
210200970318     C                   CLEAR                   EDSD20
210300970318     C                   MOVEL     '99999'       DE1496
210400990607     C                   END                                                    -- 02 --
210500980223      *
210600980223      *  Scrivo D20 da SG3
210700970318     C                   MOVE      Y3            DE7224
210800970318     C                   Z-ADD     0             WW                3 0
210900990607     C                   DO        Y3            WE                3 0          -- 02 --
211000990607      *
211100990607      *  Scrivo ogni §PUEL2 elementi
211200990607     C     WW            IFEQ      §PUEL2
211300990607     C                   MOVEL     '24'          DE4233
211400990607     C                   EXSR      WRT20
211500990607     C                   MOVEL     *BLANKS       EDSD20
211600990607     C                   Z-ADD     *ZEROS        WW
211700990607     C                   ENDIF
211800990607     C                   ADD       1             WW
211900990607     C                   MOVEL     SG3(WE)       D20(WW)
212000990607     C                   ENDDO                                                  -- 02 --
212100990607      *
212200990607      *  Se ci sono ancora dei colli scrittura nuovo record
212300990607     C     WW            IFNE      *ZEROS                                       -- 02 --
212400990607     C                   MOVEL     '24'          DE4233
212500990607     C                   EXSR      WRT20
212600990607     C                   MOVEL     *BLANKS       EDSD20
212700990607     C                   ENDIF                                                  -- 02 --
212800980220      *
212900990607     C                   ENDIF                                                  -- 01 --
213000990607      *
213100980220      * Per CCA=5 o CCA=3 e  §PTDET='N' Y2, Y3 = 0 e Y4 > 0 imposto Y4 e SG4
213200990607     C     BLPCCA        IFEQ      '5'                                          -- 01 --
213300970619     C     BLPCCA        OREQ      '3'
213400980220      *
213500990607     C     §PTDET        IFEQ      'N'                                          -- 02 --
213600970619     C     Y2            ANDEQ     0
213700970619     C     Y3            ANDEQ     0
213800970619     C     Y4            ANDNE     0
213900970619     C                   Z-ADD     Y4            DE7224
214000970619     C                   Z-ADD     0             WW                3 0
214100990607     C                   DO        Y4            WE                3 0          -- 03 --
214200990607      *
214300990607      *  Scrivo ogni §PUEL2 elementi
214400990607     C     WW            IFEQ      §PUEL2
214500990607     C                   MOVEL     '24'          DE4233
214600990607     C                   EXSR      WRT20
214700990607     C                   MOVEL     *BLANKS       EDSD20
214800990607     C                   Z-ADD     *ZEROS        WW
214900990607     C                   ENDIF
215000990607     C                   ADD       1             WW
215100990607     C                   MOVEL     SG4(WE)       D20(WW)
215200990607     C                   ENDDO                                                  -- 03 --
215300990607      *
215400990607      *  Se ci sono ancora dei colli scrittura nuovo record
215500990607     C     WW            IFNE      *ZEROS                                       -- 03 --
215600990607     C                   MOVEL     '24'          DE4233
215700990607     C                   EXSR      WRT20
215800990607     C                   MOVEL     *BLANKS       EDSD20
215900990607     C                   ENDIF                                                  -- 03 --
216000990607      *
216100990607     C                   END                                                    -- 02 --
216200990607     C                   END                                                    -- 01 --
216300980223      *
216400960912     C                   ENDSR
216500980223      *----------------------------------------------------------------
216600980223      *? TUTTI - fleggo tutti i record di BLT come trasmessi
216700980223      *----------------------------------------------------------------
216800970304     C     TUTTI         BEGSR
216900980223      *
217000980223      *  Aggiorno come trasmessi i singoli colli
217100970303     C     KBLT          CHAIN     FNBLT01L                           31
217200970303     C     *IN31         DOWEQ     '0'
217300980223      *
217400980223      *  Considero solo i colli non trasmessi al cliente
217500970303     C     BLTFTR        IFNE      'C'
217600980223      *
217700980223      *  Fleggo segnacolli come trasmessi se consegnati
217800970303     C     BLTDCM        IFGT      0
217900980223      *
218000980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
218100970303     C                   MOVEL     'C'           BLTFTR
218200970303     C                   EXCEPT    AGGTRA
218300970303     C                   END
218400970304     C                   END
218500980223      * Lettura successiva
218600970303     C     KBLT          READE     FNBLT01L                               31
218700970303     C                   END
218800980223      *
218900970304     C                   ENDSR
219000980223      *----------------------------------------------------------------
219100980223      *? REDBLT - fleggo tutti i record di BLT come trasmessi
219200980223      *----------------------------------------------------------------
219300970304     C     REDBLT        BEGSR
219400980223      *
219500970304     C                   Z-ADD     0             WCOLCO            5 0
219600970304     C                   Z-ADD     0             WCOLID            5 0
219700980223      *
219800980223      *  Aggiorno come trasmessi i singoli colli
219900970304     C     KBLT          CHAIN     FNBLT01L                           31
220000970304     C     *IN31         DOWEQ     '0'
220100980223      *
220200980223      *  Considero solo i colli non trasmessi al cliente
220300970304     C     BLTFTR        IFNE      'C'
220400980223      *
220500980223      *  Fleggo segnacolli come trasmessi se consegnati
220600970304     C     BLTDCM        IFGT      0
220700970320     C     BLTFL1        OREQ      '5'
220800970617     C     BLTFL1        OREQ      '7'
220900970617     C     BLPCCA        OREQ      '3'
221000980223      *
221100980223      *  Se chiusi con pratica IDD aggiungo 1 al relativo nr.colli
221200980223      *  altrimenti CONTROLLO SE sono colli consegnati
221300970304     C     BLTFL1        IFEQ      '5'
221400970617     C     BLTFL1        OREQ      '7'
221500970617     C     BLPCCA        OREQ      '3'
221600970304     C                   ADD       1             WCOLID
221700970304     C                   ELSE
221800970304     C                   ADD       1             WCOLCO
221900970320     C                   END
222000980223      *
222100980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
222200970304     C                   MOVEL     'C'           BLTFTR
222300970304     C                   EXCEPT    AGGTRA
222400970304     C                   END
222500970304     C                   END
222600980223      * Lettura successiva
222700970304     C     KBLT          READE     FNBLT01L                               31
222800970304     C                   END
222900980223      *
223000980223      * Se ci sono dei colli chiusi con pratica IDD scrivo TD20 per loro
223100970304     C     WCOLID        IFNE      0
223200970304     C                   MOVE      WCOLID        DE7224
223300970304     C                   EXSR      WRT20
223400980223      *
223500980223      * Se ci sono anche dei colli consegnati normalmente riscrivo SD00 e SD05
223600970304     C     WCOLCO        IFNE      0
223700081216     C                   ADD       1             WPROG
223800970304     C                   MOVEL     WPROG         DA1490
223900970304     C                   MOVEL     '9999'        DA1312
224000970304     C                   CLEAR                   WDAT
224100970304     C                   MOVEL     EDSD00        WDAT
224200970304     C                   Z-ADD     §MSNUM        STAPRG
224300970304     C                   MOVEL     'SD00'        STATPR
224400970304     C                   MOVEL     WDAT          STADAT
224500120830      *
224600120830      *  Testata messaggio  GID-PCI
224700120830     c                   exsr      write_OUTPUT
224800110315      *
224900080422     c                   eval      Righe_Dett = 'S'
225000980223      * consegna parziale
225100970304     C                   Z-ADD     1             X
225200970304     C     'CPA'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
225300970304     C     *IN32         IFEQ      '1'
225400970304     C     §PULAB        IFEQ      'S'
225500970304     C                   MOVEL     CEV(X)        DB9013
225600970304     C                   MOVEL     STS(X)        DB9011
225700970304     C                   ELSE
225800970304     C                   MOVEL     CDV(X)        DB9013
225900970304     C                   MOVEL     STV(X)        DB9011
226000970304     C                   END
226100970304     C                   MOVEL     '7  '         DB2005
226200970304     C                   MOVEL     BLPDCP        WDATHM
226300970304     C                   MOVE      VACHMC        WDATHM           12 0
226400970304     C                   MOVEL     WDATHM        DB2380
226500970304     C                   MOVEL     '203'         DB2379
226600970304     C                   END
226700980223      *
226800980223      *  Scivo record evento CONSEGNA
226900970304     C                   MOVEL     EDSD05        WDAT
227000970304     C                   Z-ADD     §MSNUM        STAPRG
227100970304     C                   MOVEL     'SD05'        STATPR
227200970304     C                   MOVEL     WDAT          STADAT
227300120830      *
227400120830      *  Testata messaggio  STS-RFF-DTM
227500120830     c                   exsr      write_OUTPUT
227600110315      *
227700080422     c                   eval      Righe_Dett = 'S'
227800980223      *  Pulisco SD20
227900970304     C                   CLEAR                   EDSD20
228000970304     C                   MOVEL     '99999'       DE1496
228100970304     C                   END
228200970304     C                   END
228300980223      *
228400980223      * Scrivo colli consegnati regolarmente
228500970304     C     WCOLCO        IFNE      0
228600970304     C                   MOVE      WCOLCO        DE7224
228700970304     C                   EXSR      WRT20
228800970304     C                   END
228900980223      *
229000970320     C                   ENDSR
229100980220      *----------------------------------------------------------------
229200980220      *? CHKBLT - Controllo se ci sono colli da trasmettere
229300980220      *----------------------------------------------------------------
229400970320     C     CHKBLT        BEGSR
229500980220      *
229600980220      *  Aggiorno come trasmessi i singoli colli
229700970320     C                   MOVEL     'N'           WTRCOL            1
229800970320     C     KBLT          CHAIN     FNBLT01L                           31
229900980220      *
230000970320     C     *IN31         DOWEQ     '0'
230100970320     C     WTRCOL        ANDEQ     'N'
230200980220      *  Considero solo i colli non trasmessi al cliente
230300970320     C     BLTFTR        IFNE      'C'
230400980220      *  Fleggo segnacolli come trasmessi se consegnati
230500970320     C     BLTDCM        IFGT      0
230600970320     C     BLTFL1        OREQ      '5'
230700970320     C                   MOVEL     'S'           WTRCOL
230800970320     C                   END
230900970320     C                   END
231000980220      * Lettura successiva
231100970320     C     KBLT          READE     FNBLT01L                               31
231200970320     C                   END
231300980220      *
231400970320     C                   ENDSR
231500980223      *----------------------------------------------------------------
231600980223      *? WRT20 - scrivo record d20
231700980223      *----------------------------------------------------------------
231800970304     C     WRT20         BEGSR
231900980223      *  Scrivo record SD20
232000960912     C                   MOVEL     EDSD20        WDAT
232100960912     C                   MOVEL     'SD20'        STATPR
232200960912     C                   Z-ADD     §MSNUM        STAPRG
232300960912     C                   MOVEL     WDAT          STADAT
232400120830      *
232500120830      *  Testata messaggio  GID-PCI
232600120830     c                   exsr      write_OUTPUT
232700110315      *
232800080422     c                   eval      Righe_Dett = 'S'
232900980223      *
233000960912     C                   ENDSR
233100980223      *----------------------------------------------------------------
233200980223      *? CARSGE - caricamento segnacollo EUROEXPRESS
233300980223      *----------------------------------------------------------------
233400980219     C     CARSGE        BEGSR
233500980219      *
233600990607     C     VACCCA        IFEQ      'A'
233700990607     C                   Z-ADD     DCDFLS        KFLS
233800990607     C                   Z-ADD     DCDLNA        KLNA
233900990607     C                   Z-ADD     DCDNRS        KNRS
234000990607     C                   Z-ADD     DCDNSC        KNSC
234100990607     C                   ELSE
234200990607     C                   Z-ADD     BLTFLS        KFLS
234300990607     C                   Z-ADD     BLTLNA        KLNA
234400990607     C                   Z-ADD     BLTNRS        KNRS
234500990607     C                   Z-ADD     BLTNSC        KNSC
234600990607     C                   ENDIF
234700050120      *
234800050120     C                   MOVEL     'E'           KTRC
234900050120      *
235000050120      * Controlla se non è Euroexpress ma un cliente normale come Disk C
235100050120     C     kfls          chain     AZORG01L
235200050120     C                   clear                   OG143
235300050120     c                   if        %Found(AZORG01L)
235400050120     C                   movel     ORGDE3        OG143
235500050120     C     §OGntw        IFne      'EEX'
235600050120     C                   z-add     1             KKUT
235700050120     C                   movel     '1B'          KCOD
235800050120     C                   movel(p)  §ptCTM        KKEY
235900050120     C     KTAB2         chain     tabel00f
236000050120     c                   if        %Found(tabel00f)
236100050120     c                   movel     tbluni        ds1B
236200050120      * se si tratta di un Disk "C" non Euroexpress --> Cliente
236300050120     c                   if        §1BF12 <> 'N'
236400050120     C                   MOVEL     'C'           KTRC
236500050120     c                   end
236600050120     c                   end
236700050120     c                   end
236800050120     c                   end
236900050120      *
237000980219     C                   MOVEL     *BLANKS       WSGN
237100980219      *
237200051017     C     KARS          CHAIN     FIARS000                           33
237300051017     C  N33              MOVEL     ARSNOT        WSGN
237400980219      *
237500980219     C                   ENDSR
237600980223      *----------------------------------------------------------------
237700980223      *? CARSGN - caricamento segnacollo
237800980223      *----------------------------------------------------------------
237900980219     C     CARSGN        BEGSR
238000980219      *
238100980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
238200960912     C                   MOVEA     *BLANKS       C35
238300960912     C                   MOVEL     *BLANKS       WSGN             35
238400980219      *  Compongo lnp se la devo passare
238500960912     C     §PULP2        IFNE      0
238600960912     C                   Z-ADD     §PULP2        WA                3 0
238700961029     C                   MOVEL     BLPFLS        WLNP              3
238800960912     C                   MOVEA     WLNP          C35(WA)
238900960912     C                   END
239000980219      *  Compongo lna se la devo passare
239100960912     C     §PULA2        IFNE      0
239200990607     C                   Z-ADD     §PULA2        WA
239300960912     C                   MOVEL     BLPLNA        WLNA              3
239400960912     C                   MOVEA     WLNA          C35(WA)
239500960912     C                   END
239600980219      *  Compongo nrs se lo devo passare
239700960912     C     §PUNR2        IFNE      0
239800990607     C                   Z-ADD     §PUNR2        WA
239900960912     C                   MOVEL     BLPNRS        WNRS              2
240000960912     C                   MOVEA     WNRS          C35(WA)
240100960912     C                   END
240200980219      *  Compongo nsc se la devo passare
240300960912     C     §PUSG2        IFNE      0
240400990607     C                   Z-ADD     §PUSG2        WA
240500990607     C     VACCCA        IFEQ      'A'
240600990604     C                   MOVEL     DCDNSC        WNSC              7
240700990604     C                   ELSE
240800990604     C                   MOVEL     BLTNSC        WNSC
240900990604     C                   END
241000960912     C                   MOVEA     WNSC          C35(WA)
241100960912     C                   END
241200980219      *  Compongo znc se la devo passare
241300960912     C     §PUZN2        IFNE      0
241400990607     C                   Z-ADD     §PUZN2        WA
241500990607     C                   MOVEL     BLPZNC        WZNC              2
241600960912     C                   MOVEA     WZNC          C35(WA)
241700960912     C                   END
241800980219      *  metto C35 in WSGN
241900960912     C                   MOVEA     C35           WSGN
242000980227      *
242100960730     C                   ENDSR
242200960927     C*----------------------------------------------------------------
242300960927     C*? CARNUM - CARICAMENTO NUMERATORE
242400960927     C*----------------------------------------------------------------
242500960927     C     CARNUM        BEGSR
242600960927     C*
242700960927     C                   Z-ADD     0             WPROG             4 0
242800960927     C                   Z-ADD     0             X                 3 0
242900960927     C                   Z-ADD     1             §MSNUM
243000960927     C                   MOVEL     'MS'          KCOD1
243100960927     C                   MOVEL     *BLANKS       KKEY1
243200960927     C                   MOVEL     '784'         KKEY1
243300960927     C     KTABE         CHAIN     EDTAB01L                           30
243400960927     C     *IN30         IFEQ      '0'
243500960927     C     TABFLG        ANDEQ     *BLANKS
243600020412     C                   MOVEL     TABUNI        EDIDSMS
243700960927     C                   ADD       1             §MSNUM
243800061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
243900061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
244000061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
244100061219     c                   move      §MSNUM        campo_4n          4 0
244200061219     c                   if        campo_4n = 0
244300061219     c                   add       1             §MSNUM
244400061219     c                   end
244500960927     C                   Z-ADD     WOGGI         §MSDAT
244600020412     C                   MOVEL     EDIDSMS       TABUNI
244700960927     C                   UPDATE    EDTAB000
244800960927     C                   END
244900980223      *
245000960927     C                   ENDSR
245100980223      *----------------------------------------------------------------
245200980223      *? *INZSR - OPERAZIONI INIZIALI
245300980223      *----------------------------------------------------------------
245400960528     C     *INZSR        BEGSR
245500080418      *
245600080418      *?  Deve leggere il nuovo TIVGD x generare EDI e fare il Download...  ?
245700080418     C     *ENTRY        PLIST
245800080418     C                   PARM                    CLIENTE           8            "0xxxyyyy"
245900080418     C                   PARM                    Tipo_File         2            "VC"
246000080418     C                   PARM                    Tipo_Scarico      2            "EW"
246100080418     C                   PARM                    Righe_Dett        1            "N"
246200110630     C                   PARM                    Esito             1            ritorno errore
246300110601      *
246400110630      * reset campo compattazione x Download file
246500110630     C                   eval      esito = '0'
246600110601     c                   eval      Compatta_80 = 'S'
246700080418     c                   eval      Righe_Dett = 'N'
246800080418      *
246900980223      * Definisco chiavi di accesso
247000080418     C     KEY_VGD01     KLIST
247100080418     C                   KFLD                    Tipo_File                      "VC"
247200080418     C                   KFLD                    CLIENTE                        "0xxxyyyy"
247300080418     C                   KFLD                    Tipo_Scarico                   "EW"
247400080418      *
247500960726     C     KTABE         KLIST
247600960726     C                   KFLD                    KCOD1
247700960726     C                   KFLD                    KKEY1
247800960528     C     KBLP          KLIST
247900960528     C                   KFLD                    KAAS
248000960528     C                   KFLD                    KLNP
248100960528     C                   KFLD                    KNRS
248200960528     C                   KFLD                    KNSP
248300990604     C     KDCD          KLIST
248400990604     C                   KFLD                    DCTAAC
248500990604     C                   KFLD                    DCTFIL
248600990604     C                   KFLD                    DCTNCA
248700960528     C     KGCP          KLIST
248800960528     C                   KFLD                    KAAS
248900960528     C                   KFLD                    KLNP
249000960528     C                   KFLD                    KNRS
249100960528     C                   KFLD                    KNSP
249200960528     C                   KFLD                    KFRG
249300050922     C     KGCp_1        KLIST
249400050922     C                   KFLD                    KAAS
249500050922     C                   KFLD                    KLNP
249600050922     C                   KFLD                    KNRS
249700050922     C                   KFLD                    KNSP
249800961204     C     KGNP          KLIST
249900961204     C                   KFLD                    KAGC
250000961204     C                   KFLD                    KFGC
250100961204     C                   KFLD                    KNGC
250200961204     C                   KFLD                    KFRG
250300961204     C                   KFLD                    KFAS
250400960528     C     KBLT          KLIST
250500960528     C                   KFLD                    KAAS
250600960528     C                   KFLD                    KLNP
250700960528     C                   KFLD                    KNRS
250800960528     C                   KFLD                    KNSP
250900970319     C     KLBL1         KLIST
251000970319     C                   KFLD                    KAAN
251100970319     C                   KFLD                    KLPN
251200970319     C                   KFLD                    KNRN
251300970319     C                   KFLD                    KNSN
251400970319     C     KLBL2         KLIST
251500970319     C                   KFLD                    KAAP
251600970319     C                   KFLD                    KLPP
251700970319     C                   KFLD                    KNRP
251800970319     C                   KFLD                    KNSP
251900060213     C     Kar4          KLIST
252000960528     C                   KFLD                    KAAS
252100960528     C                   KFLD                    KLNP
252200960528     C                   KFLD                    KNRS
252300960528     C                   KFLD                    KNSP
252400960528     C                   KFLD                    KTRC
252500960628     C     KRDE          KLIST
252600960628     C                   KFLD                    KAAS
252700960628     C                   KFLD                    KLNP
252800960628     C                   KFLD                    KNRS
252900960628     C                   KFLD                    KNSP
253000960628     C                   KFLD                    KNMC
253100970319     C                   KFLD                    KNRP1
253200961029     C     KTAB1         KLIST
253300961029     C                   KFLD                    KKUT
253400961029     C                   KFLD                    KCOD
253500970319     C     KTAB2         KLIST
253600970319     C                   KFLD                    KKUT
253700970319     C                   KFLD                    KCOD
253800970319     C                   KFLD                    KKEY
253900970319     C     KEVB          KLIST
254000970319     C                   KFLD                    KAA2
254100970319     C                   KFLD                    KLNP
254200970319     C                   KFLD                    KNRS
254300970319     C                   KFLD                    KNSP
254400970319     C                   KFLD                    KCEV
254500980204     C     KEVB1         KLIST
254600980204     C                   KFLD                    KAA2
254700980204     C                   KFLD                    KLNP
254800980204     C                   KFLD                    KNRS
254900980204     C                   KFLD                    KNSP
255000980204     C                   KFLD                    KDTV
255100980204     C                   KFLD                    KORV
255200980204     C     KEVB2         KLIST
255300980204     C                   KFLD                    KAA2
255400980204     C                   KFLD                    KLNP
255500980204     C                   KFLD                    KNRS
255600980204     C                   KFLD                    KNSP
255700140910     C     KParzial      KLIST
255800140910     C                   KFLD                    KAAS
255900140910     C                   KFLD                    KLNP
256000140910     C                   KFLD                    KNRS
256100140910     C                   KFLD                    KNSP
256200140910     C                   KFLD                    KCEV_P            3
256300051017     C     KARS          KLIST
256400980219     C                   KFLD                    KFLS
256500980219     C                   KFLD                    KLNA
256600980219     C                   KFLD                    KNRS
256700980219     C                   KFLD                    KNSC
256800980219     C                   KFLD                    KTRC
256900040607     C     KARF          klist
257000040607     C                   kfld                    KAAS
257100040607     C                   kfld                    KLNP
257200040607     C                   kfld                    KNRS
257300040607     C                   kfld                    KNSP
257400040715
257500040715     C     Kdct          klist
257600040715     C                   KFLD                    kaac
257700040715     C                   KFLD                    kfil
257800040715     C                   KFLD                    knca
257900040715      *
258000980223      * Definizione campi di work
258100980219     C     *LIKE         DEFINE    BLTNSC        KNSC
258200980219     C     *LIKE         DEFINE    BLTLNA        KLNA
258300051017     C     *LIKE         DEFINE    ARSFLS        KFLS
258400980219     C     *LIKE         DEFINE    TBLKUT        KKUT
258500961029     C     *LIKE         DEFINE    TBLCOD        KCOD
258600970319     C     *LIKE         DEFINE    TBLKEY        KKEY
258700961029     C     *LIKE         DEFINE    TABCOD        KCOD1
258800960726     C     *LIKE         DEFINE    TABKEY        KKEY1
258900960528     C     *LIKE         DEFINE    BLPAAS        KAAS
259000960528     C     *LIKE         DEFINE    BLPLNP        KLNP
259100960528     C     *LIKE         DEFINE    BLPNRS        KNRS
259200960528     C     *LIKE         DEFINE    BLPNSP        KNSP
259300060213     C     *LIKE         DEFINE    ar4TRC        KTRC
259400960528     C     *LIKE         DEFINE    GCPFRG        KFRG
259500960628     C     *LIKE         DEFINE    RDENMC        KNMC
259600970319     C     *LIKE         DEFINE    RDENRP        KNRP1
259700961204     C     *LIKE         DEFINE    GNPAGC        KAGC
259800961204     C     *LIKE         DEFINE    GNPFGC        KFGC
259900961204     C     *LIKE         DEFINE    GNPNGC        KNGC
260000961204     C     *LIKE         DEFINE    GNPFAS        KFAS
260100970319     C     *LIKE         DEFINE    LBLAAN        KAAN
260200970319     C     *LIKE         DEFINE    LBLLPN        KLPN
260300970319     C     *LIKE         DEFINE    LBLNRN        KNRN
260400970319     C     *LIKE         DEFINE    LBLNSN        KNSN
260500970319     C     *LIKE         DEFINE    LBLAAP        KAAP
260600970319     C     *LIKE         DEFINE    LBLLPP        KLPP
260700970319     C     *LIKE         DEFINE    LBLNRP        KNRP
260800970319     C     *LIKE         DEFINE    EVBAAS        KAA2
260900970319     C     *LIKE         DEFINE    EVBCEV        KCEV
261000980204     C     *LIKE         DEFINE    EVBDTV        KDTV
261100980204     C     *LIKE         DEFINE    EVBORV        KORV
261200980223      *
261300980223      * Recupero codice AS
261400960528     C                   Z-ADD     1             CODUT             1 0
261500960705     C                   CALL      'X§PARUT'
261600020412     C                   PARM                    UT§DSE0F
261700960528     C                   MOVEL     REC80         CNCR80
261800980223      *
261900980223      * Recupero data e ora x messaggio
262000960528     C                   TIME                    WHHDAT           14 0
262100960708     C                   MOVE      WHHDAT        G02DAT
262200960528     C                   MOVE      WHHDAT        DATA              6 0
262300960528     C                   MOVEL     *BLANK        G02ERR
262400960528     C                   CALL      'XSRDA8'
262500960528     C                   PARM                    WLBDA8
262600960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
262700960708     C                   MOVEL     WHHDAT        WHHMM             4 0
262800960528     C                   MOVE      WHHDAT        WANNO             2 0
262900100304     C                   Z-ADD     woggi         WOGGIymd          6 0           UDATE
263000980629      * ---------------------------------------------
263100980629      *  CARICO TABELLE
263200980629      * ---------------------------------------------
263300980223      * Caricamento Tabella eventi con IDD
263400050909      *    o tabella Giacenze/Lasciati Avviso
263500970319     C                   Z-ADD     0             X
263600050909     C                   Z-ADD     0             Xj                3 0
263700050922     C                   Z-ADD     0             Xa                3 0
263800970319     C                   Z-ADD     1             KKUT
263900970319     C                   MOVEL     '2A'          KCOD
264000970319     C                   MOVEA     *BLANKS       IDD
264100050922     C                   MOVEA     *BLANKS       GiaC
264200050926     C                   MOVEA     *BLANKS       LAvv
264300970319     C     KTAB1         CHAIN     TABEL00F                           30
264400970319     C     *IN30         DOWEQ     '0'
264500970319     C     TBLFLG        IFEQ      *BLANKS
264600970319     C                   MOVEL     TBLUNI        DS2A
264700050909      * con IDD
264800970319     C     §2AIDD        IFNE      *BLANKS
264900970319     C                   ADD       1             X
265000970319     C                   MOVEL     TBLKEY        IDD(X)
265100970319     C                   END
265200050909      *
265300050922      * Cod. Giacenza
265400050926     C                   if        §2AFTC = 'G'
265500050909     C                   add       1             Xj
265600050922     C                   MOVEL     TBLKEY        GiaC(Xj)
265700050909     C                   end
265800050922      * Lasciato Avviso
265900050926     C                   if        §2AFTC = 'A'
266000050922     C                   add       1             Xa
266100050926     C                   MOVEL     TBLKEY        LAvv(Xa)
266200050922     C                   end
266300050922      *
266400970319     C                   END
266500970319     C     KTAB1         READE     TABEL00F                               30
266600970319     C                   END
266700030926      *
266800030926     ** carico la schiera dei P.O presenti sull AS
266900030926     c                   clear                   tibs56ds
267000030926     c                   eval      i56ppo = simfel
267100030926     c                   eval      i56mod = 'POS'
267200030926     c                   call      'TIBS56R'
267300030926     c                   parm                    tibs56ds
267400030926     C                   MOVEA     SKI           Pou
267500980629      *
267600980223      * Caricamento Tabella Partner esteri
267700960528     C                   Z-ADD     0             X                 3 0
267800960528     C                   MOVEL     'PT'          TABCOD
267900960528     C     TABCOD        CHAIN     EDTAB01L                           30
268000960528     C     *IN30         DOWEQ     '0'
268100960528     C     TABFLG        IFEQ      *BLANKS
268200960528     C                   ADD       1             X
268300960528     C                   MOVEL     TABKEY        CPT(X)
268400961023     C                   MOVEL     TABKEY        CPU(X)
268500961021     C                   MOVE      '    '        CPT(X)
268600960528     C                   MOVEL     TABUNI        KSC(X)
268700960708     C                   MOVEL     TABUNI        DPT(X)
268800960528     C                   END
268900960528     C     TABCOD        READE     EDTAB01L                               30
269000960528     C                   END
269100980223      *
269200091117      * Deve comunque controllare che i codici caricati dalla PT
269300091117      *  non abbiano un altro codice di riferimento sulla 3K con cui
269400091117      *  impostare il membro.
269500091117     c                   do        x             xx
269600091117     C                   Z-ADD     1             KKUT
269700091117     C                   MOVEL     '3K'          KCOD
269800120413      *
269900120413     C                   movel(p)  KSC(xx)       KKEY
270000091117     C     KTAB2         chain     tabel00f
270100091117     c                   if        %Found(tabel00f)
270200091117      *
270300091117     c                   movel     tbluni        ds3K
270400091117     c                   if        §3Kcks <> KSC(xx)
270500091117      *  sostituisce il codice con quello del membro
270600091117     c                   move      §3Kcks        Ksc(xx)
270700091117      *
270800091117     c                   end
270900091117     c                   end
271000091117     C                   END
271100980629      *
271200980629      *  In base al codice del PARTNER reperisco identificativo
271300080418     C                   MOVE      Cliente       WCCM              7 0
271400020221     C                   Z-ADD     1             XX                3 0
271500980629     C     WCCM          LOOKUP    KSC(XX)                                32
271600020916      *
271700070503      *?  Se non trova la "PT" non deve proseguire   ?
271800070503     C     *IN32         IFEQ      '0'
271900070503     c                   move      'S'           wFINE
272000070503     c                   leaveSR
272100070503     c                   end
272200070503      *
272300980629     C     *IN32         IFEQ      '1'
272400980629      *
272500020412     C                   MOVEL     DPT(XX)       EDIDSPT
272600980629      *
272700980629     C                   MOVEL     'PU'          KCOD1
272800980707     C                   MOVEL     CPU(XX)       KKEY1
272900980707     C     KTABE         CHAIN     EDTAB01L                           30
273000980629      *
273100980629     C     *IN30         IFEQ      '0'
273200980629     C     TABFLG        ANDEQ     *BLANKS
273300020412     C                   MOVEL     TABUNI        EDIDSPU
273400980629     C                   END
273500110318      *>>>>>>>>>>
273600110318      *   Con quale traduttore deve essere scaricato e scritto il File ????
273700110318     C                   MOVEL     'PZ'          KCOD1
273800110318     C                   MOVEL     CPU(XX)       KKEY1
273900110318     C     KTABE         CHAIN     EDTAB01L
274000110318     C                   if        %Found(EDTAB01L) and tabflg = *blank
274100110318     C                   MOVEL     TABUNI        EDIDSPz
274200110405      *
274300110405      * per scrivere il file EDIFTSTA in QTEMP altrimenti lo deve lasciare chiuso
274400110405     c                   if        §PZPGMTRSS = 'INTESA'
274500110405     C                   open      EDIFTSTA
274600110405     c                   end
274700110405      *
274800110318     C                   END
274900140221      *>>>>>>>>>>
275000140221      *   Controlla la presenza della tabella NUOVA dal "PS" nata il 21/2/2014
275100140221      *    per POTER NON generare lo stato di APPUNTAMENTO/SUPERMERCATO durante la
275200140221      *   generazione del MIC o della CONSEGNA
275300140221     C                   MOVEL     'PS'          KCOD1
275400140221     C                   MOVEL     CPU(XX)       KKEY1
275500140221     C                   clear                   EDIDSPs
275600140221     C                   eval      Invio_Appuntamento_Supermercato = *Blank
275700140416     C                   eval      Invio_RFF_CU = *Blank
275800140221     C     KTABE         CHAIN     EDTAB01L
275900140221     C                   if        %Found(EDTAB01L) and tabflg = *blank
276000140221     C                   MOVEL     TABUNI        EDIDSPs
276100140221     C                   eval      Invio_Appuntamento_Supermercato = §psAPP
276200140416     C                   eval      Invio_RFF_CU = §psRCU
276300160706     C                   eval      ordinamSTD_EDIFACT_FTX_NAD = §psFTXNAD
276400161013     C                   eval      Invio_NAD_CN = §psNADcn
276500161013     C                   eval      Invio_NAD_CN_Solo_in_Delivery = §PScnSU21
276600140221     C                   END
276700110318      *>>>>>>>>>>
276800980629      * Caricamento Tabella codici eventi. Se richesta Lista Labarta e codici
276900980629      *   Labarta sono Blanks non carico rcd, lo stesso per Lista VGL.
277000980629     C                   Z-ADD     0             X                 3 0
277100980629     C                   MOVEL     '2A'          TABCOD
277200980629      *
277300980629     C     TABCOD        CHAIN     EDTAB01L                           30
277400980629     C     *IN30         DOWEQ     '0'
277500020412     C                   MOVEL     TABUNI        EDIDS2A
277600980629      *
277700980629     C                   SELECT
277800980629      *
277900980629     C     TABFLG        WHENNE    *BLANKS
278000980629      *
278100980629     C     §PULAB        WHENEQ    'S'
278200980629     C     §2ASTS        ANDEQ     *BLANKS
278300980629     C     §2ACEV        ANDEQ     *BLANKS
278400980629      *
278500980629     C     §PULAB        WHENEQ    *BLANKS
278600980629     C     §2ASTV        ANDEQ     *BLANKS
278700980629     C     §2ACDV        ANDEQ     *BLANKS
278800980629      *
278900980629     C                   OTHER
279000980629     C                   ADD       1             X
279100980629     C                   MOVEL     TABKEY        C2A(X)
279200980629     C                   MOVEL     §2ACEV        CEV(X)
279300980629     C                   MOVEL     §2ASTS        STS(X)
279400980629     C                   MOVEL     §2ACDV        CDV(X)
279500980629     C                   MOVEL     §2ASTV        STV(X)
279600980629     C                   ENDSL
279700980629      *
279800980629     C     TABCOD        READE     EDTAB01L                               30
279900980629     C                   END
280000980629      *
280100980629     C                   END
280200980629      *---------------------------------------
280300980629      *
280400980223      * Caricamento NUMERATORE
280500960927     C                   EXSR      CARNUM
280600980223      * finta read x IFTSTA
280700960528     C   01
280800960528     CANN01              READ      EDIFTSTA                               01
280900980223      *
281000980223      * Controllo se esiste EDRDE
281100980126     C                   Z-ADD     45            LUNG             15 5
281200980126     C                   MOVEL     *BLANKS       COMMAN
281300980126     C                   MOVEA     CMD(1)        COMMAN           80
281400980126     C                   CALL      'QCMDEXC'                            21
281500980126     C                   PARM                    COMMAN
281600980126     C                   PARM                    LUNG
281700980126     C     *IN21         IFEQ      '0'
281800980126     C                   OPEN      EDRDE01L
281900980126     C                   MOVEL     'S'           WTRRDE            1
282000980126     C                   END
282100980223      *
282200960528     C                   ENDSR
282300110316      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
282400120830      *  Scrive l'OUTPUT da inviare
282500110316      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
282600120830     C     write_OUTPUT  BEGSR
282700110316      *
282800120830     c                   if        §PZPGMTRSS = 'INTESA'
282900120830     C                   WRITE     IFTSTA00
283000120830     c                   elseIf    §PZPGMTRSS = 'STA94A'
283100120830     c                   exsr      Exit_segmenti
283200120830     c                   elseIf    §PZPGMTRSS = 'GELSTA'
283300120830     c                   exsr      Exit_FileGEL
283400131003     c                   elseIf    §PZPGMTRSS = 'AMZSTA'
283500131003     c                   exsr      Exit_FileAMZ
283600120830     C                   EnD
283700120830      *
283800120830     C                   ENDSR
283900120830      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
284000120830      *  Scrittura dei segmenti IFTSTA D94A
284100120830      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
284200120830     C     Exit_Segmenti BEGSR
284300120830      *
284400110318     c                   If        §PZPGMTRSS = 'STA94A'
284500110318      *
284600110318     c                   eval      Tipo_DS = STATPR
284700110318     c                   eval      Suffisso   = §PUSIG
284800110601     c                   eval      Compatta_80 = §pucompat
284900110322     c                   clear                   DsGenerica
285000110322     c                   eval      DsGenerica = STADAT
285100110322     c                   call      'TRTCT81R1'
285200110318     c                   PARM                    CLIENTE
285300110318     c                   PARM                    Suffisso          2
285400110318     c                   PARM                    Tipo_DS           4
285500110318     c                   PARM                    DsGenerica     1024
285600110322     C                   PARM                    Numero_Righe      5 0
285700110601     c                   PARM                    Compatta_80       1
285800110617     C                   PARM                    Progressivo      10
285900110621     c                   PARM                    esito             1
286000110630     C
286100110630     C                   if        esito = 'E'
286200110630     c                   exsr      Invio_Mail
286300110630     c                   end
286400110318      *
286500110318     c                   end
286600120830      *
286700120830     C                   ENDSR
286800120830     c*==================================================================*
286900120830      * Manda un File a tracciato NETEXPRESS EUROPE
287000120830     c*==================================================================*
287100120830     c     Exit_FileGEL  begsr
287200120830      *
287300120830     c                   clear                   DsGenerica
287400120830     c                   eval      Tipo_DS = STATPR
287500120830     c                   eval      DsGenerica = STADAT
287600120905     c                   call      'TRTCT81GR'
287700120830     c                   PARM                    CLIENTE
287800120830     c                   PARM                    Tipo_DS           4
287900120830     c                   PARM                    DsGenerica     1024
288000120830     C                   PARM                    Numero_Righe      5 0
288100120830     C                   PARM                    Progressivo      10
288200120830     c                   PARM                    esito             1
288300120830     C
288400120830     C                   if        esito = 'E'
288500120830     c                   exsr      Invio_Mail
288600120830     c                   end
288700110316      *
288800110316     C                   ENDSR
288900131003     c*==================================================================*
289000131003      * Manda un File a tracciato IFTSTA Particolarissimo di AMAZON
289100131003     c*==================================================================*
289200131003     c     Exit_FileAMZ  begsr
289300131003      *
289400131003     c                   clear                   DsGenerica
289500131003     c                   eval      Tipo_DS = STATPR
289600131003     c                   eval      DsGenerica = STADAT
289700131003     c                   call      'TRTCT81AR'
289800131003     c                   PARM                    CLIENTE
289900131003     c                   PARM                    Tipo_DS           4
290000131003     c                   PARM                    DsGenerica     1024
290100131003     C                   PARM                    Numero_Righe      5 0
290200131003     C                   PARM                    Progressivo      10
290300131003     c                   PARM                    esito             1
290400131003     C
290500131003     C                   if        esito = 'E'
290600131003     c                   exsr      Invio_Mail
290700131003     c                   end
290800131003      *
290900131003     C                   ENDSR
291000110630     c*==================================================================*
291100110630      * Manda un Msg x E-mail
291200110630     c*==================================================================*
291300110630     c     Invio_Mail    begsr
291400110630      *
291500110630???? C                   EVAL      Oggetto ='Problemi DOWNLD STATI EDI !!'
291600110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
291700110630     C                             STATI NON tradotto x invio su DOWNLD. -
291800110630     C                             Controllare il Download del Cliente. << '+
291900110630     c                             CLIENTE + ' >>'
292000110630      *
292100110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
292200110630     C* Inizializzo variabili
292300110630     C                   movel     *blanks       wrkEml          253
292400110630     C                   movel     *blanks       wrkMsg         5000
292500110630     C                   movel     *blanks       wrkOgg           44
292600110630     C* Valorizzo i campi della e-m@ail - indirizzo
292700110630     C                   eval      wrkEml = CED_BART
292800110630     C                   eval      wrkOgg = Oggetto
292900110630     C                   eval      wrkMsg = Messaggio
293000110630     C                   call(e)   'TRTCT00R2'
293100110630     C                   parm                    wrkEml
293200110630     C                   parm                    wrkOgg
293300110630     C                   parm                    wrkMsg
293301180223     C*
293302180223     C                   ENDSR
293303180223      * ?------------------------------------------------------------------ */
293304180223      *?   Chiodo x GEODIS Maison du Monde 0040721 o 0041186
293305180223      * ?------------------------------------------------------------------ */
293306180223     C     Chiodo_GEODIS BEGSR
293400180223      *
293401180223      *  2 codici Maison du Monde
293402180223     C                   If            vacCCM = 0040712     or
293403180223     C                                 vacCCM = 0041186
293405180223      *  Fermo Deposito = 032
293406180223     c                   if        c2a(x) = '032' and
293407180223     c                              DB9011 ='56 ' and DB9013 ='265'
293409180223     c                   eval       DB9011 ='90 '
293410180226     c                   eval       DB9013 ='267'
293412180223     c                   end
293413180226      *-
293414180223     C                   End
293415180223      *
293500110630     C                   ENDSR
293600110630      * ?------------------------------------------------------------------ */
293700110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
293800110630      * ?------------------------------------------------------------------ */
293900110630     C     *pssr         BEGSR
294000110630     C
294100110630     C                   eval      esito = 'E'
294200110630     c                   exsr      Invio_Mail
294300110630     C
294400110630     C                   ENDSR     '*CANCL'
294500110630     C
294600110316      *-----------------------------------------------------*
294700980223      *   EXCPT   x aggiornamento flag trasmissione BLT     *
294800980223      *-----------------------------------------------------*
294900970303     OFNBLT000  E            AGGTRA
295000970303     O                       BLTFTR
295100990607      *-----------------------------------------------------*
295200990607      *   EXCPT   x aggiornamento flag trasmissione DCT     *
295300990607      *-----------------------------------------------------*
295400990607     OFNDCT000  E            AGGDCT
295500990607     O                       DCTFLO
295600980126**
295700980126CHKOBJ OBJ(EDRDE01L) OBJTYPE(*FILE)
