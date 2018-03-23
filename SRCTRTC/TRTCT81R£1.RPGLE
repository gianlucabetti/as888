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
021700110630     C*---------------------------------------------------------------*
021800110630     D MsgDSP          S            256                                         Testo generico
021900110630     C*---------------------------------------------------------------*
022000110630     D SndMg01         S             10                                         Message type
022100110630     D                                     INZ('*INFO')
022200110630     D SndMg02         S             10                                         Deluvery mode
022300110630     D                                     INZ('*BREAK')
022400110630     D SndMg03         S            256                                         Message text
022500110630     D SndMg04         S             10I 0                                      Length of text
022600110630     D                                     INZ(%SIZE(SndMg03))
022700110630     D SndMg05         S             10                                         User profile
022800110630     D                                     DIM(299)
022900110630     D SndMg06         S             10I 0                                      Number of user
023000110630     D SndMg07         S             10I 0                                      Message sent indic.
023100110630     D SndMg08         S             10I 0                                      Function requested
023200110630     D SndMg10         S              1                                         Show display
023300110630     D                                     INZ('N')
023400110630     D SndMg11         S             20                                         Qualified MSGQ name
023500110630     D SndMg12         S              4                                         Name type indicator
023600110630     D                                     INZ('*USR')
023700110630      * indice di scihera
023800110630     D Jx              S              5I 0
023900110630      *
024000110630     D/COPY QSYSINC/QRPGLESRC,QUSEC
024100110630      *
024200120830     d bart_it         C                   CONST('@Brt.it;')
024300120830     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
024400020418
024500960708     D*
024600961122     D COST1           C                   CONST('IT/BAR/IT1   ')
024700060503     D CALB_Plat       C                   CONST('039IT04507990150')
024800960528     I*---------------------------------------------------------------------*
024900960528     I* FILE
025000960528     I*---------------------------------------------------------------------*
025100960528     IIFTSTA00
025200960528     I              SSIFTSTA                    STADAT
025300040607
025400040607     I* GLI INDICATORI DI TIPO RECORD x file Combinato
025500040607     IFNARBD00      11
025600040607     IFNARBK00      12
025700040607     IFIARBT00      13
025800040607     IFNARBG00      14
025900040607     IFNARBM00      15
026000040607     IFNARBV00      16
026100040607     IFNARBP00      17
026200131014     IFNARBN00      18
026300040607      *  INDICI PER:
026400040607      *  - ANNO
026500040607      *  - LINEA PARTENZA
026600040607      *  - SERIE
026700040607      *  - NUMERO SPEDIZIONE
026800040607      *  - DATA VARIAZIONE
026900040607      *  - ORA VARIAZIONE
027000000201      *---------------------------------------------------------------------*
027100000201      * Ciclo principale
027200000201      *---------------------------------------------------------------------*
027300070503      *   Se non è stata trovata la PT nella INZSR deve chiudere il PGM  ?
027400070503     C                   if        WFINE = 'S'
027500070503     c                   goto      fine
027600070503     c                   end
027700080418      * Posizionamento sul TIVGD x il nuovo ciclo di lettura
027800080418     C                   EXSR      POSFIL
027900000201      *
028000960528      * Posizionamento iniziale sul file  (prima lettura)
028100960528     C                   EXSR      REDFIL
028200000201      *
028300000201      * TESTATA Messaggio
028400000201     C                   if        WFINE <> 'S'
028500000201     C                   EXSR      TESTAMSG
028600000201     C                   endif
028700980220      *
028800980220      * Loop letture FNVAC e scrittura record con dati singola bolla
028900000201     C     WFINE         DOWNE     'S'
029000000201      *
029100000201      * Se ho scritto 999 spedizioni devo scrivere un nuovo msg
029200000201     C                   if        WPROG = 999
029300980223      * Caricamento NUMERATORE
029400960927     C                   EXSR      CARNUM
029500000201      * TESTATA Messaggio
029600000201     C                   EXSR      TESTAMSG
029700000201     C                   endif
029800000201      *
029900000201      *  DETTAGLIO Messaggio
030000000201      *
030100980220      * Eseguo preparazione dati per scrittura record SD00
030200960528     C                   EXSR      WRSD00
030300980220      * Eseguo scrittura dati record SD05 e SD00
030400980220     C                   MOVEL     'N'           WRTEVE            1
030500960528     C                   EXSR      WRSD05
030600980220      *
030700980220      * Scrivo tipi record successivi solo se ho scritto SD00 e SD05
030800980220     C     WRTEVE        IFEQ      'S'
030900150611      *
031000150611      *   Vero Standard in sequenza corretta in prova solo su cliente SDKCOS
031100160706      *    e PARTNER France Express.  (prima deve essere segm FTX poi NAD+AP:Firma)
031200160706     C                   if        ordinamSTD_EDIFACT_FTX_NAD = 'S'
031300150611     C                   EXSR      WRSD15
031400150611     C                   EXSR      WRSD10
031500150611     c                   else
031600150611      *
031700980220      * Eseguo scrittura dati record SD10
031800960528     C                   EXSR      WRSD10
031900960528      * Eseguo scrittura dati record SD15
032000960528     C                   EXSR      WRSD15
032100150611     c                   end
032200150611      *
032300960528      * Eseguo scrittura dati record SD20
032400960528     C                   EXSR      WRSD20
032500980220     C                   END
032600100304      *
032700100304      * Se arrivato qui tutto OK
032800100304      * Aggancia il record fisico per certificare che è andato tutto bene
032900100304      *  e che ha elaborato correttamente il record trasformandolo in EDI.
033000100304     c                   clear                   elab10           10 0
033100100304     c     NrRECvgd      chain     tiVGD00F
033200100304     c                   if        %Found(tiVGD00F)
033300100304     c                   eval      Elab10 = WoggiYMD * 10000 + wHHMM
033400100304     c                   movel     Elab10        vgdPRG
033500100304     c                   update    tiVGDfis
033600100304     c                   end
033700980220      * Lettura successiva
033800961007     C     NEWREC        TAG
033900960528     C                   EXSR      REDFIL
034000000201     C                   ENDDO
034100980220      * Fine pgm.
034200121010      *   se si tratta di un traduttore NON EDIFACT deve far saltare al CL la parte
034300121010      *    successiva di trasmissione
034400121010     c                   if        §PZPGMTRSS = 'GELSTA'
034500131003     c                                or
034600131003     c                             §PZPGMTRSS = 'AMZSTA'
034700121010     c                   eval      Righe_Dett = 'N'
034800121010     c                   end
034900121010      *
035000070503     c     fine          tag
035100000201     C                   eval      *inlr = *on
035200080418      *----------------------------------------------------------------
035300080418      *? POSFIL - Si Posiziona x la LETTURA PRIMO RECORD
035400080418      *----------------------------------------------------------------
035500080418     C     POSFIL        BEGSR
035600080418      *
035700080418      *? Si posiziona con il tipo VC e cliente + l'invio EDI...  ?
035800080418     C     KEY_VGD01     setll     tiVGD01L
035900080418      *
036000080418     C                   ENDSR
036100980220      *----------------------------------------------------------------
036200980220      *? REDFIL - LETTURA PROSSIMO RECORD
036300980220      *----------------------------------------------------------------
036400960528     C     REDFIL        BEGSR
036500980220      *
036600990326     C     LEGGI         TAG
036700990326      *
036800080418      * Lettura file VAC
036900080418     C     KEY_VGD01     reade     tiVGD01L                               30
037000990326      *
037100000201     C                   If        *IN30 = *on
037200000201     C                   MOVEL     'S'           WFINE             1
037300110318      *
037400110318     c                   If        §PZPGMTRSS = 'STA94A'
037500110322     C                               and  Numero_Righe > 1
037600120830     c                               or
037700120830     c                             §PZPGMTRSS = 'GELSTA'
037800131003     c                               or
037900131003     c                             §PZPGMTRSS = 'AMZSTA'
038000120830      *
038100110405      * Se c'è stato qualche evento da inviare che quindi ha scritto
038200110405      *   chiude il messaggio altrimenti esegue un ROLLBACK
038300110405     c                   if        Righe_Dett = 'S'
038400120830      *
038500110318      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
038600110318      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
038700110412     c                   move      §MSNUM        fine04            4
038800110318     c                   eval      STATPR = 'FINE'
038900110411     c                   eval      STADAT = fine04
039000120830     c                   exsr      write_OUTPUT
039100110318     c                   eval      Righe_Dett = 'N'
039200110405      *
039300110405     c                   else
039400110405      * deve uscire senza avere fatto nulla
039500110405     c                   eval      STATPR = 'RLBK'
039600120830     c                   exsr      write_OUTPUT
039700110405     c                   end
039800110405      *
039900110318     c                   end
040000121010      *
040100000201     C                   Else
040200090219      *
040300090219      *? DEVE LEGGERE solo i record con TIPO invio "EW" da mandare via EDI
040400090219     c                   if        vgdTSC <> 'EW'
040500090219     c                   goto      LEGGI
040600090219     c                   end
040700990326      *
040800080418      *? Una volta letto il TIVGD imposta la relativa DS del VAC...  ?
040900080418     c                   eval      fnVACds = vgdDTA
041000080418      *
041100080418      *? poi aggiorna come già letto il record ...  ?
041200080418     c                   eval      VGDSTO ='S'
041300100304     c                   eval      VGDCNT = VGDCNT + 1
041400080418      *
041500080418      *?                ====================  ?
041600080418     c                   update    tivgd000
041700080418      *?                ====================  ?
041800080418      *
041900051116      *  Pulizia campi riferimento di giacenze della precedente lettura
042000051116     c                   clear                   GCPAGC
042100051116     c                   clear                   GCPFGC
042200051116     c                   clear                   GCPNGC
042300051116      *
042400990326      * Non elaboro codice anomalia IDD3 trasmesso dall'arrivo
042500000201     C                   if        VACCCA = '5'
042600990326     C                   GOTO      LEGGI
042700000201     C                   endif
042800990922      *
042900990922      * Non elaboro SE NON TROVO RCD DI FNBLP
043000990922     C                   Z-ADD     VACAAS        KAAS
043100990922     C                   Z-ADD     VACLNP        KLNP
043200990922     C                   Z-ADD     VACNRS        KNRS
043300990922     C                   Z-ADD     VACNSP        KNSP
043400990922     C     KBLP          CHAIN     FNBLP01L                           31
043500000201     C   31              GOTO      LEGGI
043600000201      *
043700000201     C                   Endif
043800980220      *
043900960528     C                   ENDSR
044000000201      *----------------------------------------------------------------
044100000201      *? TESTAMSG - SCRIVO TESTATA MESSAGGIO
044200000201      *----------------------------------------------------------------
044300000201     C     TESTAMSG      BEGSR
044400000201      *
044500000201      * Eseguo scrittura dati record ST00
044600000201     C                   EXSR      WRST00
044700000201      * Eseguo scrittura dati record ST05
044800000201     C                   EXSR      WRST05
044900000201      * Eseguo scrittura dati record ST10
045000000201     C***                EXSR      WRST10
045100000201      * Eseguo scrittura dati record ST15
045200000201     C                   EXSR      WRST15
045300000201      *
045400000201     C                   ENDSR
045500980629      *----------------------------------------------------------------
045600980629      *? WRST00 - Scrittura record partner
045700980629      *----------------------------------------------------------------
045800960528     C     WRST00        BEGSR
045900980629      *
046000980629      *  Pulisco dati DS
046100960528     C                   CLEAR                   EDST00
046200980629      *  Mittente: Bartolini  (ITBAR + AS..)
046300960528     C                   MOVEL     'ITBAR'       WMITT             8
046400960917     C                   MOVE      '005'         WMITT             8
046500960530     C                   MOVEL     WMITT         TA0004
046600960729     C                   MOVEL     'ZZ'          TA0007
046700980220      *
046800980629      *  In base al codice del PARTNER reperisco identificativo
046900980629     C                   MOVEL     CPT(XX)       TA0010
047000980629      *
047100980220      *  Utilizzo qualificatore x destinatario msg.
047200960827     C                   MOVEL     §PTMSQ        TAA007
047300980220      *  Data/Ora invio messagio: adesso
047400971031     C                   MOVEL     WOGGI         TA0017
047500960528     C                   MOVEL     WHHMM         TA0019
047600980220      *  Codice documento - nome documento
047700960528     C                   MOVEL     '784'         TA1001
047800960913     C                   MOVEL     '      '      TA1000
047900960708     C                   MOVEL     WHHDAT        TA1004                         DATA/ORA -> MSG
048000960528     C                   MOVEL     '9  '         TA1225
048100960528     C                   MOVEL     '137'         TA2005
048200980220      *  Data messaggio: oggi
048300960528     C                   MOVEL     WOGGI         TA2380
048400960528     C                   MOVEL     '102'         TA2379
048500100512      *
048600100512      *    Se si vuole la data con l'orario
048700100512     c                   if        §pudate203 ='S'
048800100512     C                   MOVE      Whhmm         TA2380
048900100512     C                   MOVEL(p)  '203'         TA2379
049000100512     c                   end
049100100512      *
049200980220      *  Scivo record IFTSTA
049300960705     C                   CLEAR                   WDAT
049400960705     C                   MOVEL     EDST00        WDAT
049500960726     C                   Z-ADD     §MSNUM        STAPRG
049600960726     C                   MOVEL     'ST00'        STATPR
049700110318     C                   MOVEL     WDAT          STADAT
049800120830      *
049900120830      *  Testata messaggio  UNB-BGM-DTM
050000120830     c                   exsr      write_OUTPUT
050100110315      *
050200960528     C                   ENDSR
050300980220      *----------------------------------------------------------------
050400980220      *? WRST05 - Scrittura record partner
050500980220      *----------------------------------------------------------------
050600960708     C     WRST05        BEGSR
050700980220      *
050800960708     C                   CLEAR                   EDST05
050900980220      *  Ship from:
051000960712     C                   MOVEL     *BLANKS       WDAT
051100960708     C                   MOVEL     'SF '         TE3035
051200970221     C                   MOVEL     §PTPLT        TE3036
051300980220      *  Scivo record IFCSUM
051400960708     C                   CLEAR                   WDAT
051500960708     C                   MOVEL     EDST05        WDAT
051600960726     C                   Z-ADD     §MSNUM        STAPRG
051700960712     C                   MOVEL     'ST05'        STATPR
051800960708     C                   MOVEL     WDAT          STADAT
051900120830      *
052000120830      *  Testata messaggio  NAD-SF
052100120830     c                   exsr      write_OUTPUT
052200980220      *  Ship to:
052300960712     C                   MOVEL     *BLANKS       WDAT
052400960708     C                   CLEAR                   EDST05
052500960708     C                   MOVEL     'ST '         TE3035
052600970221     C                   MOVEL     COST1         TE3036
052700060503      *
052800060503      *  x CALBERSON --> GEODIS
052900060503      *      imposta la Platform che ci hanno chiesto di utilizzare
053000060503      *      x identificare diversamente dai dati di DUNLOP altrimenti
053100060503      *      non riescono a dividerli nel loro sistema.
053200060503      *  da impostare il §PTLOC ??????
053300060503     c                   if        §PTKSC = 493298
053400060503     C                   clear                   TE3036
053500060503     C                   MOVEL     CALB_Plat     TE3036
053600060503     c                   end
053700060503      *
053800980220      *  Scivo record IFCSUM
053900960708     C                   CLEAR                   WDAT
054000960708     C                   MOVEL     EDST05        WDAT
054100960726     C                   Z-ADD     §MSNUM        STAPRG
054200960712     C                   MOVEL     'ST05'        STATPR
054300960708     C                   MOVEL     WDAT          STADAT
054400120830      *
054500120830      *  Testata messaggio  NAD-ST
054600120830     c                   exsr      write_OUTPUT
054700980220      *
054800960708     C                   ENDSR
054900980220      *----------------------------------------------------------------
055000980220      *? WRST10 - Scrittura record partner
055100980220      *----------------------------------------------------------------
055200960528     C     WRST10        BEGSR
055300980220      *
055400980220      *  Pulisco dati DS
055500960712     C                   MOVEL     *BLANKS       WDAT
055600960528     C                   CLEAR                   EDST10
055700980220      *  Se devo ritornare imposto AFB + nr.messaggio
055800960726     C                   MOVEL     'AFB'         TB1153
055900960726     C                   MOVEL     §MSNUM        TB1154
056000980220      *  Scivo record IFCSUM
056100960726     C                   CLEAR                   WDAT
056200960726     C                   MOVEL     EDST10        WDAT
056300960726     C                   Z-ADD     §MSNUM        STAPRG
056400960726     C                   MOVEL     'ST10'        STATPR
056500960726     C                   MOVEL     WDAT          STADAT
056600120830      *
056700120830      *  Testata messaggio  RFF-LOC
056800120830     c                   exsr      write_OUTPUT
056900980220      *
057000960528     C                   ENDSR
057100980220      *----------------------------------------------------------------
057200980220      *? WRST15 - Scrittura record partner
057300980220      *----------------------------------------------------------------
057400960528     C     WRST15        BEGSR
057500980220      *
057600980220      *  Pulisco dati DS
057700960712     C                   MOVEL     *BLANKS       WDAT
057800960528     C                   CLEAR                   EDST15
057900980220      *
058000960528     C                   ENDSR
058100980220      *----------------------------------------------------------------
058200980220      *? WRSD00 - Scrittura record partner - dettaglio
058300980220      *----------------------------------------------------------------
058400960528     C     WRSD00        BEGSR
058500980220      *
058600980220      *  Pulisco dati DS
058700960712     C                   MOVEL     *BLANKS       WDAT
058800960528     C                   CLEAR                   EDSD00
058900980220      *  Controllo se la bolla figlia con bolla originale su stesso AS
059000980220      *  non invio le date di consegna
059100970319     C                   Z-ADD     VACAAS        KAAN
059200970319     C                   Z-ADD     VACLNP        KLPN
059300970319     C                   Z-ADD     VACNRS        KNRN
059400970319     C                   Z-ADD     VACNSP        KNSN
059500970319     C     KLBL1         CHAIN     FNLBL01L                           31
059600970319     C     *IN31         IFEQ      '0'
059700030926     C     LBLLPO        LOOKUP    Pou                                    31
059800970319     C   31              GOTO      NEWREC
059900970319     C                   END
060000980220      *
060100980220      *  Imposto numero spedizione originale cliente/partner
060200961022     C                   MOVEL     *BLANKS       WNRSPE           35
060300960528     C                   Z-ADD     VACAAS        KAAS
060400960528     C                   Z-ADD     VACLNP        KLNP
060500960528     C                   Z-ADD     VACNRS        KNRS
060600960528     C                   Z-ADD     VACNSP        KNSP
060700040305      * La regola è:
060800040305      *  si deve restituire il Nostro Nr.spedizione nel CNI invece
060900060213      *   dal ar4 --> deve essere impostato l'RFF+AGE ossia Rif.Partner
061000080418     c                   eval      cni_VACAAS = VACAAS
061100080418     c                   eval      cni_VACLNP = VACLNP
061200080418     c                   eval      cni_VACNRS = VACNRS
061300080418     c                   eval      cni_VACNSP = VACNSP
061400080418     c                   eval      cni_VACLNA = VACLNA
061500040305      *
061600040305      * quindi occorre impostare diversamente i campi della DS00 e DS05
061700960708     C                   MOVEL     'E'           KTRC
061800060213     C     Kar4          CHAIN     fiar401L                           31
061900980220      *
062000100722      * se non richiesta forzatura del Riferimento numerico da restituire
062100100722     c                   if        *in31 = '0' and §ptSGN =' '
062200060213     C                   MOVEL     ar4NOT        Wnrspe
062300960821     C                   ELSE
062400980220      *
062500060213      * Se non trovo ar4NOT utilizzo in cascata Rif.Mittente Alfa e numerico
062600980220      * (????) Segnalo se manca ricezione EDI e sped. bollettata a mano
062700980220     C                   MOVEL     'S'           WSD15             1
062800960821     C     KBLP          CHAIN     FNBLP01L                           31
062900960821     C     *IN31         IFEQ      '0'
063000100722      * se non richiesta forzatura del Riferimento numerico da restituire
063100960821     C     BLPRMA        IFNE      *BLANKS
063200100722     C     §ptsgn        andeq     *BLANKS
063300040305     C                   MOVEL     BLPRMA        Wnrspe
063400960821     C                   ELSE
063500040305     C                   MOVEL     BLPRMN        Wnrspe
063600961007     C                   ENDIF
063700960821     C                   ENDIF
063800960708     C                   END
063900100209      * Default
064000100209      *  se non richiesta l'inversione del dato CNI nell'AGE
064100100209     c                   if        §ptTRD <> 'S'
064200100209     C                   MOVEL(p)  CNI_spe       DA1004
064300100209      *  inverte con l'AGE
064400100209     c                   elseIF    §ptTRD  = 'S'
064500100209     C                   MOVEL(p)  Wnrspe        DA1004
064600100209     c                   end
064700100304      *
064800100304      * Se non ho impostato DA1004 leggo rcd successivo
064900100304     C     DA1004        IFEQ      *BLANKS
065000100304     C                   GOTO      NEWREC
065100100304     C                   ENDIF
065200980220      *
065300960528     C                   ENDSR
065400980220      *----------------------------------------------------------------
065500980220      *? WRSD05 - Scrittura record partner - dettaglio
065600980220      *----------------------------------------------------------------
065700960528     C     WRSD05        BEGSR
065800980220      *
065900980220      *  Pulisco dati DS
066000960712     C                   MOVEL     *BLANKS       WDAT
066100960528     C                   CLEAR                   EDSD05
066200980220      *
066300980220      *  Se ho l'obbligo di ritornaglielo scrivo nr. CMR altrimenti nr. sped.
066400980220     C     §PURFF        IFEQ      'C'
066500980220     C                   EXSR      NUMCMR
066600980220     C                   ELSE
066700961022     C                   MOVEL     'AGE'         DB1153
066800100209      * Default
066900100209      *  se non richiesta l'inversione del dato CNI nell'AGE
067000100209     c                   if        §ptTRD <> 'S'
067100961022     C                   MOVEL     WNRSPE        DB1154
067200100209      *  inverte con il CNI
067300100209     c                   elseIF    §ptTRD  = 'S'
067400100209     C                   MOVEL     CNI_spe       DB1154
067500100209     c                   end
067600140416      *
067700980220     C                   END
067800140416      *------------------------------------------------------*
067900140416      *   se si deve inviare forzatamente
068000140416      *     il riferimento numerico RFF+CU oltre all'AGE
068100140416      *  NON è in alternativa al riferimento CMR è sempre in più
068200140416      **
068300140416     C                   if        Invio_RFF_CU = 'S'
068400140416     C                               and BLPrmn > 0
068500140416     C                   eval      DB1153A = 'CU'
068600140416     C                   eval      DB1154A = %Trim(%Editc(blpRMN:'Z'))
068700140416     c                   end
068800980220      *------------------------------------------------------*
068900980220      *  Se la bolla è una mamma controllo se è stato effettuato un evento IDD
069000970319     C                   MOVEL     'N'           WTREVE            1
069100970319     C                   Z-ADD     VACAAS        KAAP
069200970319     C                   Z-ADD     VACLNP        KLPP
069300970319     C                   Z-ADD     VACNRS        KNRP
069400970319     C                   Z-ADD     VACNSP        KNSP
069500970319     C     KLBL2         CHAIN     FNLBL02L                           31
069600980220      *
069700980220      *  Scorro schiera eventi IDD e controllo se ce ne sono
069800980220     C     *IN31         IFEQ      '0'
069900970319     C                   Z-ADD     1             X
070000980220     C     IDD(X)        DOWNE     *BLANKS
070100970319     C     X             ANDLE     200
070200970319     C     WTREVE        ANDEQ     'N'
070300970319     C                   MOVEL     IDD(X)        KCEV
070400970319     C                   Z-ADD     VACAAS        KAA2
070500970319     C                   Z-ADD     VACLNP        KLNP
070600970319     C                   Z-ADD     VACNRS        KNRS
070700970319     C                   Z-ADD     VACNSP        KNSP
070800990810     C     KEVB          CHAIN     FNEVB04L                           31
070900980220     C  N31              MOVEL     'S'           WTREVE
071000980220     C   31              ADD       1             X
071100980220     C                   END
071200980220      *
071300980220     C                   END
071400980220      *------------------------------------------------------*
071500990604      *  APERTURA C.A.
071600990604     C                   MOVEL     *BLANKS       WELACA            1
071700990604     C     VACCCA        IFEQ      'A'                                          -- 01 --
071800040715      *
071900040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
072000040715      * SIA COME MAMMA CHE COME FIGLIA
072100040715     c                   clear                   fidn12ds
072200040715     c                   eval      i12aas = kAAS
072300040715     c                   eval      i12lnp = kLNP
072400040715     c                   eval      i12nrs = kNRS
072500040715     c                   eval      i12nsp = kNSP
072600040715     c                   eval      i12fel = 'E'
072700040715     c                   eval      i12fan = 'E'
072800040715     c                   eval      i12fch = 'E'
072900040715     c                   eval      i12fpt = 'E'
073000040715     c                   call      'FIDN12R'
073100040715     c                   parm                    fidn12ds
073200040715     c                   z-add     *zeros        ii                2 0
073300040715      **
073400040715      * se trovata almeno una CA
073500040715     c                   if        o12nca > 0
073600040715     c                   dow       ii <  o12nca
073700040715     c                   add       1             ii
073800040715     c                   movea     skkey(ii)     dskey
073900040715     C                   z-add     dsaac         kaac
074000040715     C                   z-add     dsfil         kfil
074100040715     C                   z-add     dsnca         knca
074200040715     c     kdct          chain(n)  FNDCT000
074300040715     c                   If        %found(fndct01l)
074400040715     C                   MOVEL     DCTFLO        DDCT01
074500040715     C     §DCTtrpt      ifeq      *BLANKS
074600040715     C                   MOVEL     'S'           WELACA
074700040715     C                   leave
074800040715     C                   endif
074900040715     C                   endif
075000040715      *
075100040715     c                   enddo
075200040715     c                   endif
075300990604      *
075400990604     C     WELACA        IFEQ      'S'                                          -- 02 --
075500990604      *  Determino la causale evento e controllo se reso codificato in tabella
075600991019      *   IDD per mancanze EuroExpress ed Europolitan; RFD per Avarie e Ammanchi
075700991019      *    EuroExpress; per Avarie e Ammanchi Europolitan non trasmetto nulla.
075800990604     C                   Z-ADD     1             X
075900991019     C                   SETOFF                                       32
076000990604      *
076100991019     C                   SELECT
076200991019      *
076300991019     C     DCTTAD        WHENNE    '01'                                         EuroExpress
076400991019     C     DCTTAD        ANDNE     '21'                                         mancanze
076500990604     C     'RFD'         LOOKUP    C2A(X)                                 32
076600991019      *
076700991019     C                   OTHER                                                  EuroExpress altro
076800990604     C     'IDD'         LOOKUP    C2A(X)                                 32
076900991019     C                   ENDSL
077000990604      *
077100990604     C     *IN32         IFEQ      '1'                                          -- 03 --
077200990604     C     §PULAB        IFEQ      'S'
077300990604     C                   MOVEL     CEV(X)        DB9013
077400990604     C                   MOVEL     STS(X)        DB9011
077500990604     C                   ELSE
077600990604     C                   MOVEL     CDV(X)        DB9013
077700990604     C                   MOVEL     STV(X)        DB9011
077800990604     C                   ENDIF
077900990604     C                   MOVEL     '7  '         DB2005
078000990604     C                   MOVEL     VACDCM        WDATHM
078100990604     C                   MOVE      VACHMC        WDATHM           12 0
078200990604     C                   MOVEL     WDATHM        DB2380
078300990604     C                   MOVEL     '203'         DB2379
078400990604      *  Scrivo record SD00 e SD05
078500990604     C                   EXSR      WRT0E5
078600990604     C                   ENDIF                                                  -- 03 --
078700990604      *
078800990604     C                   ENDIF                                                  -- 02 --
078900990608     C                   GOTO      FIND05
079000990604     C                   ENDIF                                                  -- 01 --
079100990604      *------------------------------------------------------*
079200980220      *  Controllo se è stata effettuata un consegna parziale
079300970319     C                   Z-ADD     VACAAS        KAAS
079400970319     C                   Z-ADD     VACLNP        KLNP
079500970319     C                   Z-ADD     VACNRS        KNRS
079600970319     C                   Z-ADD     VACNSP        KNSP
079700970319     C     KBLP          CHAIN     FNBLP01L                           31
079800980220      *
079900980220     C     BLPDCP        IFGT      0
080000960729     C     BLPDCM        ANDEQ     0
080100980220      *
080200980220      *  Controllo se devo trasmettere i colli
080300980220     C     WTREVE        IFEQ      'S'
080400970320     C                   EXSR      CHKBLT
080500980220      *  Se  non ci sono colli da trasmettere passo al record successivo
080600980220     C     WTRCOL        IFEQ      'N'
080700980220     C                   GOTO      NEWREC
080800980220     C                   END
080900980220     C                   END
081000980220      *
081100960528     C                   Z-ADD     1             X
081200970617     C                   SETOFF                                       32
081300980220      *
081400970617     C     §PTDET        IFEQ      'M'
081500970320     C     BLPCCA        ANDEQ     '5'
081600970320     C     §PTDET        OREQ      'M'
081700970320     C     WTREVE        ANDEQ     'S'
081800970320     C     'PID'         LOOKUP    C2A(X)                                 32
081900970320     C                   ELSE
082000970320     C     'P  '         LOOKUP    C2A(X)                                 32
082100970320     C                   END
082200980220      *
082300980220     C   32§PULAB        IFEQ      'S'
082400960528     C                   MOVEL     CEV(X)        DB9013
082500960612     C                   MOVEL     STS(X)        DB9011
082600961022     C                   ELSE
082700961022     C                   MOVEL     CDV(X)        DB9013
082800961022     C                   MOVEL     STV(X)        DB9011
082900980220     C                   END
083000980220      *
083100960528     C                   MOVEL     '7  '         DB2005
083200960528     C                   MOVEL     BLPDCP        DB2380
083300960528     C                   MOVEL     '102'         DB2379
083400100512      *
083500100512      *    Se si vuole la data con l'orario
083600100512     c                   if        §pudate203 ='S'
083700140910     C                   MOVE      '0001'        DB2380
083800140910     C                   MOVEL(p)  '203'         DB2379
083900140910      *
084000140910      *  Controlla direttamente sugli EVENTI la presenza di Data e Ora
084100140910      *   da impostare se richiesta l'informazione completa dell'orario.
084200140910     C                   eval         KCEV_P = 'P  '
084300140910      *-
084400140910     C     KParzial      setgt     fnEVB04L
084500140910     C     KParzial      readpe    fnEVB04L
084600140910     c                   dow       not %EoF(fnEVB04L)
084700140910     c                   if           evbDEV = blpDCP
084800140910     C                   movel     evbDEV        WDATHM
084900140910     C                   move      evbHEV        WDATHM
085000140910     C                   movel     WDATHM        DB2380
085100140910     c                   leave
085200140910     c                   end
085300140910     C     KParzial      readpe    fnEVB04L
085400140910     c                   enddo
085500140910      *
085600100512     c                   end
085700100512      *
085800980220      *  Scrivo record SD00 e SD05
085900980220     C                   EXSR      WRT0E5
086000960528     C                   GOTO      FIND05
086100980220     C                   END
086200990604      *-------------------------------------------------------------*
086300050922      * aggancia sempre la giacenza se c'è stata giacenza e non è consegnata
086400050922     C     vacDAG        ifGT      0                                             -- 0A --
086500050922     C     vacDCM        andEQ     0
086600050922     C                   Z-ADD     VACAAS        KAAS
086700050922     C                   Z-ADD     VACLNP        KLNP
086800050922     C                   Z-ADD     VACNRS        KNRS
086900050922     C                   Z-ADD     VACNSP        KNSP
087000050922     c                   Z-ADD     0             KFRG
087100050922     C     KGCP          CHAIN     tigcp01L                           31
087200050922     c                   if        %Found(tigcp01L)                              -- 0B --
087300050922
087400050922      * imposta la data Distinta/udate apertura giacenza fuori distinta
087500050922     c                   z-add     GCPDXD        vacDAG
087600050922
087700980220      *  Se la data più alta è di giacenza invio data apertura giacenza
087800970617     C     VACDAG        IFGT      VACDLA                                       -- 01 --
087900960528     C     VACDAG        ANDGT     VACDCM
088000960528     C     VACDAG        ANDGT     0
088100980220      *  Controllo se il codice apertura giacenza è codificato in tabella
088200960528     C                   Z-ADD     1             X
088300960528     C     GCPCMC        LOOKUP    C2A(X)                                 32
088400970617     C     *IN32         IFEQ      '1'                                          -- 02 --
088500980220      *
088600980220     C     §PULAB        IFEQ      'S'                                          -- 03 --
088700960528     C                   MOVEL     CEV(X)        DB9013
088800960612     C                   MOVEL     STS(X)        DB9011
088900980220     C                   ELSE                                                   -- 03 --
089000961022     C                   MOVEL     CDV(X)        DB9013
089100961022     C                   MOVEL     STV(X)        DB9011
089200980220     C                   END                                                    -- 03 --
089300980220      *
089400960528     C                   MOVEL     '7  '         DB2005
089500050909      * ----->
089600050919      *   Imposta la Data e le ore 18:00 fisse. Prima prendeva sempre la data
089700050919      *   apertura/riapertura della giacenza invece,
089800050919      *    questa è la data della distinta di consegna da ARB oppure
089900050919      *    la UDATE se si è aperta giacenza non passando da distinta.
090000050919     C                   movel     gcpDXD        WDATHM
090100050909     C                   move      '1800'        WDATHM
090200050909     C                   movel     WDATHM        DB2380
090300050922      *
090400050922      * Controlla se è stato fatto un evento di giacenza senza aver
090500050922      *  successivamente riaperto Giacenza ... prende questo come Data/Ora
090600050922     C     KGCP_1        setgt     fnEVB04L
090700050922     C     KGCP_1        readpe    fnEVB04L
090800050922     c                   dow       not %EoF(fnEVB04L)
090900050922     c     evbCEV        lookup    GiaC                                   31
091000050922     c                   if        *in31 = *on
091100050922     c                   if        evbDEV > gcpDXD
091200050922     C                   movel     evbDEV        WDATHM
091300050922     C                   move      evbHEV        WDATHM
091400050922     C                   movel     WDATHM        DB2380
091500050922     c                   end
091600050922     c                   leave
091700050922     c                   end
091800050922     C     KGCP_1        readpe    fnEVB04L
091900050922     c                   enddo
092000120830      *
092100050909     C                   movel     '203'         DB2379
092200050922      *
092300980220      *  Scrivo record SD00 e SD05
092400980220     C                   EXSR      WRT0E5
092500980220     C                   GOTO      FIND05
092600970617     C                   END                                                    -- 02 --
092700970617     C                   END                                                    -- 01 --
092800050922
092900050922     c                   end                                                    -- 0B --
093000050922     c                   end                                                    -- 0A --
093100980220      *-------------------------------------------------------------*
093200980220      *  La spedizione ha avuto lasciato avviso
093300980220     C     VACDLA        IFGT      VACDCM
093400960528     C     VACDLA        ANDGT     0
093500980220     C*  Controllo se il codice lasciato avviso è codificato in tabella
093600960528     C                   Z-ADD     1             X
093700960528     C     'AVV'         LOOKUP    C2A(X)                                 32
093800980220      *
093900960528     C     *IN32         IFEQ      '1'
094000980220      *
094100961022     C     §PULAB        IFEQ      'S'
094200960528     C                   MOVEL     CEV(X)        DB9013
094300960612     C                   MOVEL     STS(X)        DB9011
094400961022     C                   ELSE
094500961022     C                   MOVEL     CDV(X)        DB9013
094600961022     C                   MOVEL     STV(X)        DB9011
094700961022     C                   END
094800980220      *
094900960528     C                   MOVEL     '7  '         DB2005
095000050926     C                   movel     VACDLA        WDATHM
095100050926     C                   move      1800          WDATHM
095200050926     C                   movel     WDATHM        DB2380
095300050926     C                   MOVEL     '203'         DB2379
095400050922      *
095500050922      * Controlla se è stato fatto un evento di L.AVV. posteriore
095600050922      *  a quello presente nel vacDLA ... prende questo come Data/Ora
095700050922     C                   Z-ADD     VACAAS        KAAS
095800050922     C                   Z-ADD     VACLNP        KLNP
095900050922     C                   Z-ADD     VACNRS        KNRS
096000050922     C                   Z-ADD     VACNSP        KNSP
096100050922     C     KGCP_1        setgt     fnEVB04L
096200050922     C     KGCP_1        readpe    fnEVB04L
096300050922     c                   dow       not %EoF(fnEVB04L)
096400050926     c     evbCEV        lookup    LAvv                                   31
096500050922     c                   if        *in31 = *on
096600050922     c                   if        evbDEV >= vacDLA
096700050922     C                   movel     evbDEV        WDATHM
096800050922     C                   move      evbHEV        WDATHM
096900050922     C                   movel     WDATHM        DB2380
097000050922     C                   MOVEL     '203'         DB2379
097100050922     c                   end
097200050922     c                   leave
097300050922     c                   end
097400050922     C     KGCP_1        readpe    fnEVB04L
097500050922     c                   enddo
097600050922
097700980220      *  Scrivo record SD00 e SD05
097800980220     C                   EXSR      WRT0E5
097900980220     C                   GOTO      FIND05
098000960528     C                   END
098100970617     C                   END                                                    -- 01 --
098200980220      *-------------------------------------------------------------*
098300081117      *  -> STATUS 20/210 Appuntamento (SUPERMERCATO)
098400081117      * Non è un evento ma una particolarità di consegna
098500081117      *  aggiunto anche per appuntamento (Booking ins)
098600081117     c                   eval      Bolla_variata  = 'N'
098700081117      *
098800140221      *   Solo se NON negato l'INVIO sulla tab.PT(PS)
098900140221     C                   if        Invio_Appuntamento_Supermercato <>'N'
099000081117     c                   if        VACTC1 = 'S' or VACTC2 = 'S' or
099100081117     c                             VACTC1 = 'A' or VACTC2 = 'A'
099200081117     c                   Exsr      STATUS_20_210
099300081117     c                   end
099400140221     c                   end
099500081117      *
099600081117      *-------------------------------------------------------------*
099700980220      *  La spedizione è stata chiusa per: RESO - IDD - CONSEGNA
099800970617     C     VACDCM        IFGT      0                                            -- 01 --
099900970617     C     VACCCA        ANDNE     '7'
100000970617     C     VACCCA        ANDNE     'P'
100100970617     C     BLPDCM        ORGT      0
100200980220      *
100300980220      *---------------------------------*
100400980220      *  RESO
100500970617     C     VACCCA        IFEQ      '2'                                          -- 02 --
100600960712     C     VACCCA        OREQ      '6'
100700980220      *
100800980220      *  Controllo se reso codificato in tab.
100900960528     C                   Z-ADD     1             X
101000980220     C     'RES'         LOOKUP    C2A(X)                                 32      ??? RESO
101100970617     C     *IN32         IFEQ      '1'                                          -- 03 --
101200961022     C     §PULAB        IFEQ      'S'
101300960528     C                   MOVEL     CEV(X)        DB9013
101400960612     C                   MOVEL     STS(X)        DB9011
101500961022     C                   ELSE
101600961022     C                   MOVEL     CDV(X)        DB9013
101700961022     C                   MOVEL     STV(X)        DB9011
101800961022     C                   END
101900960528     C                   MOVEL     '7  '         DB2005
102000960528     C                   MOVEL     VACDCM        WDATHM
102100960528     C                   MOVE      VACHMC        WDATHM           12 0
102200960528     C                   MOVEL     WDATHM        DB2380
102300960528     C                   MOVEL     '203'         DB2379
102400980220      *  Scrivo record SD00 e SD05
102500980220     C                   EXSR      WRT0E5
102600980220     C                   GOTO      FIND05
102700970617     C                   END                                                    -- 03 --
102800980220      *---------------------------------*
102900970617     C                   ELSE                                                   -- 02 --
103000980220      *---------------------------------*
103100980220      *  COLLI MANCANTI - IDD -
103200120830      *
103300970617     C     VACCCA        IFEQ      '5'                                          -- 03 --
103400970617     C     VACCCA        OREQ      '3'
103500970617     C     VACCCA        OREQ      '7'
103600970319     C     WTREVE        OREQ      'S'
103700980220      *
103800980220      *  Controllo se reso codificato in tab.
103900970121     C                   Z-ADD     1             X
104000970121     C     'IDD'         LOOKUP    C2A(X)                                 32     IDD
104100980220      *
104200970617     C     *IN32         IFEQ      '1'                                          -- 04 --
104300970121     C     §PULAB        IFEQ      'S'
104400970121     C                   MOVEL     CEV(X)        DB9013
104500970121     C                   MOVEL     STS(X)        DB9011
104600970121     C                   ELSE
104700970121     C                   MOVEL     CDV(X)        DB9013
104800970121     C                   MOVEL     STV(X)        DB9011
104900970121     C                   END
105000970121     C                   MOVEL     '7  '         DB2005
105100970121     C                   MOVEL     VACDCM        WDATHM
105200970121     C                   MOVE      VACHMC        WDATHM           12 0
105300970121     C                   MOVEL     WDATHM        DB2380
105400970121     C                   MOVEL     '203'         DB2379
105500980220      *  Scrivo record SD00 e SD05
105600980220     C                   EXSR      WRT0E5
105700980220     C                   GOTO      FIND05
105800970617     C                   END                                                    -- 04 --
105900980220      *---------------------------------*
106000970617     C                   ELSE                                                   -- 03 --
106100980319      *---------------------------------*
106200980319      * Se VACCCA='C' --> ho ricevuto dall'arrivo e devo trasmettere
106300980319      *                   al partner un evento di messa in consegna
106400980319     C     VACCCA        IFEQ      'C'                                          -- 04 --
106500040707      *---------------------------------*
106600040707      *  Prima del MIC        -> STATUS 20/210 SUPERMERCATO
106700040707      *
106800040707      * Non è un evento ma una prticolarità di consegna
106900040721      *  aggiunto anche per appuntamento (Booking ins)
107000081111      *    SOLO SE NON E'STAT MANDATA PRIMA:
107100081112     c                   if        Bolla_variata  = 'N'
107200081112      *
107300140221      *   Solo se NON negato l'INVIO sulla tab.PT(PS)
107400140221     C                   if        Invio_Appuntamento_Supermercato <>'N'
107500081111     c                   if        VACTC1 = 'S' or VACTC2 = 'S' or
107600081111     c                             VACTC1 = 'A' or VACTC2 = 'A'
107700081111     c                   Exsr      STATUS_20_210
107800081111     c                   end
107900140221     c                   end
108000081119      *
108100081111     c                   endif
108200040707      *---------------------------------*
108300980319     C                   Z-ADD     1             X
108400980319     C     'MIC'         LOOKUP    C2A(X)                                 32
108500980319      *
108600980319     C     *IN32         IFEQ      '1'                                          -- 05 --
108700980319     C     §PULAB        IFEQ      'S'
108800980319     C                   MOVEL     CEV(X)        DB9013
108900980319     C                   MOVEL     STS(X)        DB9011
109000980319     C                   ELSE
109100980319     C                   MOVEL     CDV(X)        DB9013
109200980319     C                   MOVEL     STV(X)        DB9011
109300980319     C                   END
109400980319     C                   MOVEL     '7  '         DB2005
109500980319     C                   MOVEL     VACDCM        WDATHM
109600980319     C                   MOVE      VACHMC        WDATHM           12 0
109700980319     C                   MOVEL     WDATHM        DB2380
109800980319     C                   MOVEL     '203'         DB2379
109900980319      *  Scrivo record SD00 e SD05
110000980319     C                   EXSR      WRT0E5
110100980319     C                   GOTO      FIND05
110200980319     C                   END                                                    -- 05 --
110300980319     C                   ELSE                                                   -- 04 --
110400980220      *---------------------------------*
110500980220      *  CONSEGNA TOTALE
110600970617     C                   Z-ADD     1             X
110700960528     C     'CON'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
110800980220      *
110900980319     C     *IN32         IFEQ      '1'                                          -- 05 --
111000961022     C     §PULAB        IFEQ      'S'
111100961022     C                   MOVEL     CEV(X)        DB9013
111200961022     C                   MOVEL     STS(X)        DB9011
111300961022     C                   ELSE
111400961022     C                   MOVEL     CDV(X)        DB9013
111500961022     C                   MOVEL     STV(X)        DB9011
111600961022     C                   END
111700960528     C                   MOVEL     '7  '         DB2005
111800960528     C                   MOVEL     VACDCM        WDATHM
111900960528     C                   MOVE      VACHMC        WDATHM           12 0
112000960528     C                   MOVEL     WDATHM        DB2380
112100960528     C                   MOVEL     '203'         DB2379
112200980220      *  Scrivo record SD00 e SD05
112300980220     C                   EXSR      WRT0E5
112400980220     C                   GOTO      FIND05
112500980319     C                   END                                                    -- 05 --
112600980220      *---------------------------------*
112700980319     C                   END                                                    -- 04 --
112800980319     C                   END                                                    -- 03 --
112900970617     C                   END                                                    -- 02 --
113000980220      *---------------------------------*
113100980220      *  La spedizione è stata chiusa per: IDD PARTENZA
113200980220     C                   ELSE
113300980220      *---------------------------------*
113400980220      * Chiusura bolla x IDD in partenza
113500970617     C     VACCCA        IFEQ      '7'                                          -- 02 --
113600970617     C     VACCCA        OREQ      'P'                                          -- 02 --
113700970617     C                   Z-ADD     1             X
113800980204     C                   Z-ADD     VACAAS        KAA2
113900980204     C                   Z-ADD     VACLNP        KLNP
114000980204     C                   Z-ADD     VACNRS        KNRS
114100980204     C                   Z-ADD     VACNSP        KNSP
114200990810     C                   Z-ADD     WOGGI         KDTV
114300980204     C                   TIME                    KORV
114400990810     C     KEVB1         SETGT     FNEVB05L
114500990810     C     KEVB2         READPE    FNEVB05L                               31
114600980220      *
114700980204     C     EVBCEV        LOOKUP    C2A(X)                                 32     IDD
114800980220      *
114900970617     C     *IN32         IFEQ      '1'                                          -- 03 --
115000970617     C     §PULAB        IFEQ      'S'
115100970617     C                   MOVEL     CEV(X)        DB9013
115200970617     C                   MOVEL     STS(X)        DB9011
115300970617     C                   ELSE
115400970617     C                   MOVEL     CDV(X)        DB9013
115500970617     C                   MOVEL     STV(X)        DB9011
115600970617     C                   END
115700970617     C                   MOVEL     '7  '         DB2005
115800970617     C                   MOVEL     VACDCM        WDATHM
115900970617     C                   MOVE      VACHMC        WDATHM           12 0
116000970617     C                   MOVEL     WDATHM        DB2380
116100970617     C                   MOVEL     '203'         DB2379
116200980220      *  Scrivo record SD00 e SD05
116300980220     C                   EXSR      WRT0E5
116400980220     C                   GOTO      FIND05
116500970617     C                   END                                                    -- 03 --
116600970617     C                   END                                                    -- 02 --
116700980220      *---------------------------------*
116800970617     C                   END
116900980220      *
117000960528     C     FIND05        ENDSR
117100040604      *-------------------------------------------------------------*
117200040604      *? STATUS_20_210      << SUPERMERCATO >>
117300040604      *----------------------------------------------------------------
117400040604     C     STATUS_20_210 Begsr
117500040604      *
117600081112     c                   eval      variata    = 'N'
117700081112      *
117800040607      * Si deve prendere l'ultima variazione del solo tipo record ARBG
117900040607      *  appartenente ad una causale CR, CP, CS, CD
118000040607     C                   Z-ADD     VACAAS        KAAS
118100040607     C                   Z-ADD     VACLNP        KLNP
118200040607     C                   Z-ADD     VACNRS        KNRS
118300040607     C                   Z-ADD     VACNSP        KNSP
118400140221     c                   clear                   salva_data
118500140221     c                   clear                   salva_ora
118600140218      *
118700131014     c     kARF          setgt     fiarbf2c
118800131014     c                   setoff                                       14
118900140218      *
119000131014     c     kARF          readpe    fiarbf2c
119100131014     c                   dow       not %eof(fiarbf2c)
119200040607      *
119300040607      * solo x tipo record FNARBG00 (variazione bolla)
119400080922     c                   if        *in14 = *on
119500080922      *
119600081111      ** con causale particolare
119700140221     c                   if        G_arbCVB ='CR' or G_arbCVB ='CS' or
119800140221     c                             G_arbCVB ='CD'
119900140221      *
120000140221      * Se presente una variazione anche se NON Appuntamento/Supermercato memorizzo
120100140221      *   ...può servire in futuro
120200140221     c                   eval      Bolla_variata  = 'S'
120300140221     ****
120400140221      *  INVECE:
120500140221      *   X dire che si tratti di una bolla VARIATA come APPUNTAMENTO/SUPERMERCATO
120600140221      *    occorre percorrere a ritroso i records di variazione x prendere il primo
120700140221      *    (fra gli ultimi) variato a causa dell'appuntamento o supermercato
120800140221     c                   if        G_arbtc1 <> 'A' and G_arbtc2 <> 'A' and
120900140221     c                             G_arbtc1 <> 'S' and G_arbtc2 <> 'S'
121000140221     ****
121100140221     c                   eval      salva_data = G_ARBdtv
121200140221     c                   eval      salva_ora  = G_ARBorv
121300140221     ****
121400140221     ****************
121500081111      *  Solo se la variazione è avvenuta in giornata viene inviata altrimenti non
121600081111      *   deve essere presa in considerazione:
121700140221     c*********          if        G_arbDTV = wOggi
121800140221     c************       eval      variata        = 'S'
121900140221     c************       leave
122000140221     c*********          endIf
122100140221      *******   SPOSTATE FUORI DAL CICLO
122200140221     ****************
122300140221     ****
122400140221     c                   else
122500140221     ****  solo se aveva preso una data di variazione nella lettura precedente
122600140221     c                   if        salva_data > 0
122700140221     c                   LEAVE
122800140221     c                   end
122900140221     c                   endif
123000140221     ****
123100040607     c                   end
123200040607     c                   end
123300080922      *
123400131014     c                   setoff                                       14
123500131014     c     kARF          readpe    fiarbf2c
123600040607     c                   enddo
123700140221     ****
123800140221     ****   PORTATE QUI Fuori Ciclo Variazioni
123900140221      *  Solo se la variazione è avvenuta in giornata viene inviata altrimenti
124000140221      *   NON deve essere presa in considerazione:
124100140221     c                   if        salva_Data > 0
124200140221      *
124300140221     c                   if        salva_Ora <= vacHMC and salva_Data <= vacDCM
124400140221     c                   eval      variata        = 'S'
124500140221     c                   endIf
124600140221      *
124700140221     c                   endIf
124800140221     ****
124900140221     ****
125000140221     ****
125100140221     ****
125200140221     ****
125300040607      *
125400040607     c                   if        variata = 'S'
125500040607      * data e ora variazione
125600140221     C******             MOVEL     G_arbORV      Wora              4 0
125700140221     C                   MOVEL     salva_ORA     Wora              4 0
125800040607     C                   MOVE      Wora          WDATHM           12 0
125900140221     C******             MOVEL     G_arbDTV      WDATHM
126000140221     C                   MOVEL     salva_Data    WDATHM
126100040607     c                   else
126200040607      * data spedizione
126300040607     C                   MOVEL     vacaas        Wdata             8 0
126400040607     C                   MOVE      vacmgs        Wdata
126500040607     C                   MOVE      '0800'        WDATHM           12 0
126600040607     C                   MOVEL     Wdata         WDATHM
126700040607     c                   end
126800040607      *
126900040607      *  20+210 --> segnala se consegnata a un Supermercato
127000040608     C                   MOVEL     '20 '         DB9011
127100040608     C                   MOVEL     '210'         DB9013
127200040607     C                   MOVEL     '7  '         DB2005
127300040607     C                   MOVEL     WDATHM        DB2380
127400040607     C                   MOVEL     '203'         DB2379
127500040607      *
127600040607      *  Scrivo record SD00 e SD05 prima dei 2 records della consegna
127700040607     C                   EXSR      WRT0E5
127800040607      *
127900040607      * e nel FTX la data di consegna richiesta se presente
128000040607      *  Pulisco dati DS
128100040607     c                   if        vacdcr <> *zero
128200040607     C                   MOVEL     *BLANKS       WDAT
128300040607     C                   CLEAR                   EDSD15
128400040607      *
128500040607      *   Note generiche ='AAI'
128600040607     C                   MOVEL     'AAI'         DD4451
128700040607     c                   select
128800040607     c                   when      vactcr = *blank
128900040607     C                   eval      DD4440 = 'ON ' +
129000040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
129100040607     c                             %editw(vachcr:'  :  ')
129200040607     c                   when      vactcr = 'P'
129300040607     C                   eval      DD4440 = 'BEFORE ' +
129400040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
129500040607     c                             %editw(vachcr:'  :  ')
129600040607     c                   when      vactcr = 'D'
129700040607     C                   eval      DD4440 = 'AFTER ' +
129800040607     c                             %editw(vacdcr:'    /  /  ') + ' ' +
129900040607     c                             %editw(vachcr:'  :  ')
130000040607     c                   endsl
130100040607      *  Scrivo record dati note
130200040607     C                   MOVEL     EDSD15        WDAT
130300040607     C                   Z-ADD     §MSNUM        STAPRG
130400040607     C                   MOVEL     'SD15'        STATPR
130500040607     C                   MOVEL     WDAT          STADAT
130600120830      *
130700120830      *  Testata messaggio  FTX-
130800120830     c                   exsr      write_OUTPUT
130900110315      *
131000080422     c                   eval      Righe_Dett = 'S'
131100040607     C                   End
131200040607      *
131300040604     C                   Endsr
131400980220      *-------------------------------------------------------------*
131500980220      *? NUMCMR - Scrivo numero CMR
131600980220      *----------------------------------------------------------------
131700980220     C     NUMCMR        BEGSR
131800980220      *
131900980220     C                   MOVEL     '784'         DB1153
132000980220     C     WTRRDE        IFEQ      'S'
132100980220      *
132200980220     C     §PTCM1        IFEQ      'S'
132300980220     C     §PTNF1        OREQ      'S'
132400980220     C                   Z-ADD     VACAAS        KAAS
132500980220     C                   Z-ADD     VACLNP        KLNP
132600980220     C                   Z-ADD     VACNRS        KNRS
132700980220     C                   Z-ADD     VACNSP        KNSP
132800980220     C                   MOVEL     'TD1154'      KNMC
132900980220     C                   Z-ADD     1             KNRP1
133000980220     C     KRDE          CHAIN     EDRDE01L                           31
133100980220     C  N31              MOVEL     RDEVAL        DB1154
133200980220     C                   END
133300980220      *
133400980220     C                   END
133500980220      *
133600980220     C                   ENDSR
133700980220      *-------------------------------------------------------------*
133800980220      *? WRT0E5 - Scrittura records d00 e d05
133900980220      *----------------------------------------------------------------
134000980220     C     WRT0E5        BEGSR
134100120830      **
134200980220      *  Scrivo tipo record SD00
134300980220     C                   ADD       1             WPROG
134400980220     C                   MOVEL     WPROG         DA1490
134500980220     C                   MOVEL     '9999'        DA1312
134600980220     C                   CLEAR                   WDAT
134700980220     C                   MOVEL     EDSD00        WDAT
134800980220     C                   Z-ADD     §MSNUM        STAPRG
134900980220     C                   MOVEL     'SD00'        STATPR
135000980220     C                   MOVEL     WDAT          STADAT
135100120830      *
135200120830      *  Testata messaggio  CNI-
135300120830     c                   exsr      write_OUTPUT
135400110315      *
135500980220      *  Scrivo tipo record SD05
135600980220     C                   MOVEL     EDSD05        WDAT
135700980220     C                   Z-ADD     §MSNUM        STAPRG
135800980220     C                   MOVEL     'SD05'        STATPR
135900980220     C                   MOVEL     WDAT          STADAT
136000120830      *
136100120830      *  Testata messaggio  STS-RFF-DTM
136200120830     c                   exsr      write_OUTPUT
136300110316      *
136400980220     C                   MOVEL     'S'           WRTEVE            1
136500080422     c                   eval      Righe_Dett = 'S'
136600980220      *
136700980220     C                   ENDSR
136800980220      *-------------------------------------------------------------*
136900980220      *? WRSD10 - Scrittura record partner - dettaglio
137000980220      *----------------------------------------------------------------
137100980220     C     WRSD10        BEGSR
137200101028      *
137300101028      *  solo chi vuole ricevere la firma del firmatario
137400101028     C                   IF        §PUfirma <>'N'
137500980220      *
137600960712     C                   MOVEL     *BLANKS       WDAT
137700960528     C                   CLEAR                   EDSD10
137800101028      *
137900101028      *  Per Schneider questo segmento NON DEVE essere inviato
138000101028      *
138100980220      *  Controllo se esiste firmatario
138200960528     C                   MOVEL     '1'           KTRC
138300060213     C     Kar4          CHAIN     fiar401L                           31
138400960528     C     *IN31         IFEQ      '0'
138500960528     C                   MOVEL     'AP '         DC3035
138600060213     C                   MOVEL     ar4NOT        DC3036
138700980220      *  Scrivo record dati firmatario
138800960705     C                   MOVEL     EDSD10        WDAT
138900960729     C                   Z-ADD     §MSNUM        STAPRG
139000960705     C                   MOVEL     'SD10'        STATPR
139100960705     C                   MOVEL     WDAT          STADAT
139200120830      *
139300120830      *  Testata messaggio  NAD-
139400120830     c                   exsr      write_OUTPUT
139500110316      *
139600080422     c                   eval      Righe_Dett = 'S'
139700960528     C                   END
139800980220      *
139900101028     c                   end
140000101028      *
140100960528     C                   ENDSR
140200980220      *-------------------------------------------------------------*
140300980220      *? WRSD15 - Scrittura record partner - dettaglio
140400980220      *----------------------------------------------------------------
140500960528     C     WRSD15        BEGSR
140600980220      *
140700980220      *  Pulisco dati DS
140800960712     C                   MOVEL     *BLANKS       WDAT
140900960528     C                   CLEAR                   EDSD15
141000980220      *
141100980220      *  Controllo se ci sono delle note di consegna
141200980220     C     VACDCM        IFNE      0                                            -- 01 --
141300980204     C                   MOVEL     *BLANKS       RDEVAL
141400980220      *
141500980220     C     WTRRDE        IFEQ      'S'                                          -- 02 --
141600980204     C                   Z-ADD     VACAAS        KAAS
141700980204     C                   Z-ADD     VACLNP        KLNP
141800980204     C                   Z-ADD     VACNRS        KNRS
141900980204     C                   Z-ADD     VACNSP        KNSP
142000980204     C                   MOVEL     'DD4440'      KNMC
142100980204     C                   Z-ADD     1             KNRP1
142200980204     C     KRDE          CHAIN     EDRDE01L                           31
142300980220      *
142400980220     C     *IN31         IFEQ      '0'                                          -- 03 --
142500980204     C                   MOVEL     'AAI'         DD4451
142600980204     C                   MOVEL     RDEVAL        DD4440
142700980204     C                   DELETE    EDRDE000
142800980220      * controllo se c'è proseguimento
142900980204     C                   Z-ADD     2             KNRP1
143000980204     C     KRDE          CHAIN     EDRDE01L                           31
143100980220      *
143200980220     C     *IN31         IFEQ      '0'                                          -- 04 --
143300980204     C                   MOVE      'AAI'         DD4451
143400980204     C                   MOVE      RDEVAL        DD4440
143500980204     C                   DELETE    EDRDE000
143600980220     C                   END                                                    -- 04 --
143700980220      *
143800980220      *  Scrivo record dati note
143900980204     C                   MOVEL     EDSD15        WDAT
144000980204     C                   Z-ADD     §MSNUM        STAPRG
144100980204     C                   MOVEL     'SD15'        STATPR
144200980204     C                   MOVEL     WDAT          STADAT
144300120830      *
144400120830      *  Testata messaggio  FTX-
144500120830     c                   exsr      write_OUTPUT
144600110315      *
144700080422     c                   eval      Righe_Dett = 'S'
144800980220     C                   END                                                    -- 03 --
144900980220     C                   END                                                    -- 02 --
145000980220      *-------------------------------
145100980220     C                   ELSE                                                   -- 01 --
145200980220      *-------------------------------
145300980220      *
145400980220      *  Se  è una giacenza cerco le note giacenza
145500961204     C                   Z-ADD     GCPAGC        KAGC
145600961204     C                   Z-ADD     GCPFGC        KFGC
145700961204     C                   Z-ADD     GCPNGC        KNGC
145800961204     C                   Z-ADD     10            KFAS
145900050309     C     KGNP          CHAIN     tignp01L                           31
146000980220     C     *IN31         IFEQ      '0'                                          -- 02 --
146100961204     C                   MOVEL     'AAI'         DD4451
146200961204     C                   MOVEL     GNPDMC        DD4440
146300961204     C*  Scivo record dati note giacenza
146400961204     C                   MOVEL     EDSD15        WDAT
146500961204     C                   Z-ADD     §MSNUM        STAPRG
146600961204     C                   MOVEL     'SD15'        STATPR
146700961204     C                   MOVEL     WDAT          STADAT
146800120830      *
146900120830      *  Testata messaggio  FTX-
147000120830     c                   exsr      write_OUTPUT
147100110316      *
147200080422     c                   eval      Righe_Dett = 'S'
147300980220     C                   END                                                    -- 02 --
147400980220     C                   END                                                    -- 01 --
147500961204     C*
147600960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
147700980220     C     WSD15         IFEQ      'S'                                          -- 01 --
147800960821     C                   MOVEL     *BLANKS       WDAT
147900960821     C                   CLEAR                   EDSD15
148000980220     C                   END                                                    -- 01 --
148100980220      *
148200960528     C                   ENDSR
148300960528     C*-------------------------------------------------------------*
148400960528     C*? WRSD20 - Scrittura record partner - dettaglio
148500980219      *----------------------------------------------------------------
148600960528     C     WRSD20        BEGSR
148700980219      *
148800980219      *  Pulisco dati DS
148900960912     C                   MOVEA     *BLANKS       SG1
149000960912     C                   MOVEA     *BLANKS       SG2
149100970318     C                   MOVEA     *BLANKS       SG3
149200970619     C                   MOVEA     *BLANKS       SG4
149300960912     C                   MOVEL     *BLANKS       WDAT
149400960528     C                   CLEAR                   EDSD20
149500960528     C                   MOVEL     '99999'       DE1496
149600960729     C                   MOVE      VACNCL        DE7224
149700980220      *
149800990604      *  Se è stata aperta C.A. elaboro a parte
149900990604     C     WELACA        IFEQ      'S'
150000990604     C                   EXSR      COLCA
150100990604     C                   GOTO      ED20
150200990604     C                   ENDIF
150300990604      *
150400990604      *  Se sto effettuando la consegna totale di una bolla con codice
150500980219      *  consegna anomala = '5' su cui è stata effettuata una consegna
150600980219      *  parziale risfleggo eventuali colli trasmessi e chiusi con IDD
150700980220      *
150800980220     C     BLPCCA        IFEQ      '5'
150900970319     C     WTREVE        OREQ      'S'
151000980220      *
151100970319     C     BLPDCM        IFGT      0
151200970318     C     BLPDCP        ANDGT     0
151300970318     C                   EXSR      LIBIDD
151400970318     C                   END
151500980220      *
151600970319     C                   END
151700980220      *
151800980220      *  Se tabellato devo sempre inviare il dettaglio segnacolli
151900980220      *  Carico tutto
152000970304     C     §PTDET        IFEQ      'S'                                          --- 02 ---
152100960912     C                   EXSR      CARCOL
152200980220     C                   EXSR      WRTCOL
152300960912     C                   END
152400980220      *
152500980223      *  Se tabellato devo inviare dettaglio solo per consegne parziali
152600980223      *  Carico non consegnati
152700970304     C     §PTDET        IFEQ      'N'                                          --- 02 ---
152800970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
152900980223      *
153000980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
153100980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
153200970303     C     BLPCCA        OREQ      '5'
153300970617     C     BLPCCA        OREQ      '3'
153400970619     C     VACCCA        OREQ      '7'
153500970619     C     VACCCA        OREQ      'P'
153600970319     C     WTREVE        OREQ      'S'
153700960912     C                   EXSR      CARCOL
153800980220     C                   EXSR      WRTCOL
153900960912     C                   ELSE
154000980223      *
154100980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
154200970304     C     BLPDCM        IFNE      0
154300970304     C                   EXSR      TUTTI
154400970304     C                   END
154500960912     C                   EXSR      WRT20
154600970304     C                   END                                                    --- 03 ---
154700970304     C                   END                                                    --- 02 ---
154800980223      *
154900980223      *  Se previsto da tabella che non devo mai passare il
155000980223      *  numero dei colli scrivo D20
155100970304     C     §PTDET        IFEQ      'M'                                          --- 02 ---
155200980223      *  Per consegne parziali o IDD loop su dettaglio segnacolli
155300970304     C     BLPDCP        IFGT      BLPDCM                                       --- 03 ---
155400980223      *
155500980223      *  In caso di spedizione chiusa con pratica IDD sempre nel caso di invio
155600980223      *  solo colli x consegne parziali, scrivo solo i colli con FL1 = '5'
155700970304     C     BLPCCA        OREQ      '5'
155800970617     C     BLPCCA        OREQ      '3'
155900970619     C     VACCCA        OREQ      '7'
156000970619     C     VACCCA        OREQ      'P'
156100970319     C     WTREVE        OREQ      'S'
156200970304     C                   EXSR      REDBLT
156300970304     C                   ELSE
156400980223      *
156500980223      *  nel caso in cui non abbia effettuato consegne parziali scrivo TD20
156600970304     C     BLPDCM        IFEQ      0
156700970304     C                   EXSR      TUTTI
156800970304     C                   END
156900970304     C                   EXSR      WRT20
157000970304     C                   END                                                    --- 03 ---
157100970304     C                   END                                                    --- 02 ---
157200980223      *
157300990604     C     ED20          ENDSR
157400980220      *----------------------------------------------------------------
157500980220      *? LIBIDD - Controllo se devo sfleggare colli con IDD
157600980220      *----------------------------------------------------------------
157700970318     C     LIBIDD        BEGSR
157800980219      *
157900980219      *  Lettura BLT e caricamento colli
158000970318     C     KBLT          CHAIN     FNBLT01L                           31
158100980219      *  rislego i colli da trasmettete al cliente
158200980220     C     *IN31         DOWEQ     '0'
158300980220      *
158400970318     C     BLTFTR        IFEQ      'C'
158500970318     C     BLTDCM        ANDGT     0
158600970318     C     BLTFL1        ANDEQ     '5'
158700970318     C                   MOVEL     ' '           BLTFTR
158800970318     C                   EXCEPT    AGGTRA
158900970318     C                   END
159000980219      * Lettura successiva
159100970318     C     KBLT          READE     FNBLT01L                               31
159200970318     C                   END
159300980219      *
159400970318     C                   ENDSR
159500980219      *----------------------------------------------------------------
159600980219      *? CARCOL - Routine caricamento colli da dettag.
159700980219      *----------------------------------------------------------------
159800960912     C     CARCOL        BEGSR
159900980219      *
160000050413      *  ---------------------
160100050413      *  Legenda dei contatori:   Y1 = Colli Consegnati
160200050413      *  ----------------------   Y2 = Colli Non Consegnati
160300050413      *                           Y3 = Colli Chiusi con IDD
160400050413      *                           Y4 = Colli Negativi con IDD
160500050413      *
160600960912     C                   Z-ADD     0             Y1                5 0
160700960912     C                   Z-ADD     0             Y2                5 0
160800970318     C                   Z-ADD     0             Y3                5 0
160900970619     C                   Z-ADD     0             Y4                5 0
161000980219      *
161100980220      *  Lettura BLT e caricamento colli nelle schiere
161200960912     C     KBLT          CHAIN     FNBLT01L                           31
161300980223      *
161400980220     C     *IN31         DOWEQ     '0'                                          -- 01 --
161500980219      *
161600980219      *  Considero solo i colli non trasmessi al cliente
161700980220     C     BLTFTR        IFNE      'C'                                          -- 02 --
161800980220      *
161900980220      *-------------------------------------------------
162000980219      *  Controllo se devo caricare colli consegnati
162100960912     C     BLTDCM        IFGT      0
162200970303     C     BLTFL1        ANDEQ     ' '
162300970303     C     §PTDET        ANDEQ     'S'
162400970617     C     BLPCCA        ANDNE     '3'
162500980219      *
162600980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
162700980219     C     §PUBS1        IFEQ      'E'
162800980219     C                   EXSR      CARSGE                                       EUROEXPRESS
162900980219     C                   ELSE
163000980219     C                   EXSR      CARSGN                                       BARTOLINI
163100980219     C                   END
163200980219      *
163300980219     C     WSGN          IFNE      *BLANKS
163400960912     C                   ADD       1             Y1
163500960912     C                   MOVEL     WSGN          SG1(Y1)
163600960912     C                   END
163700980219     C                   END
163800980219      *
163900980220      *-------------------------------------------------
164000980219      *  Controllo se devo caricare colli non consegnati
164100960912     C     BLTDCM        IFEQ      0
164200970304     C     §PTDET        ANDNE     'M'
164300970619     C     VACCCA        ANDNE     '7'
164400970304     C     BLTFL1        OREQ      '5'
164500960913     C     §PTDET        ANDNE     'M'
164600970617     C     BLPDCM        ANDGT     0
164700970617     C     BLTFL1        OREQ      '7'
164800970617     C     §PTDET        ANDNE     'M'
164900970617     C     BLTDCM        ANDGT     0
165000970617     C     VACCCA        ANDNE     'P'
165100970617     C     BLPCCA        OREQ      '3'
165200970617     C     §PTDET        ANDNE     'M'
165300970617     C     BLPDCM        ANDGT     0
165400980219      *
165500980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
165600980219     C     §PUBS1        IFEQ      'E'
165700980219     C                   EXSR      CARSGE                                       EUROEXPRESS
165800980219     C                   ELSE
165900980219     C                   EXSR      CARSGN                                       BARTOLINI
166000980219     C                   END
166100980219      *
166200980219     C     WSGN          IFNE      *BLANKS
166300960912     C                   ADD       1             Y2
166400960912     C                   MOVEL     WSGN          SG2(Y2)
166500960912     C                   END
166600980219     C                   END
166700980220      *
166800980220      *-------------------------------------------------
166900980220      *  Controllo se devo caricare colli chiusi con IDD x evento PID
167000970318     C     BLTFL1        IFEQ      '5'
167100970318     C     §PTDET        ANDNE     'M'
167200970318     C     BLPDCM        ANDEQ     0
167300970617     C     BLTFL1        OREQ      '7'
167400970617     C     §PTDET        ANDNE     'M'
167500970617     C     VACCCA        ANDEQ     'P'
167600980220      *
167700980220      *  Se ha segnacollato il cliente carico in schiera i colli cons.
167800980219     C     §PUBS1        IFEQ      'E'
167900980219     C                   EXSR      CARSGE                                       EUROEXPRESS
168000980219     C                   ELSE
168100980219     C                   EXSR      CARSGN                                       BARTOLINI
168200980219     C                   END
168300980219      *
168400980219     C     WSGN          IFNE      *BLANKS
168500970318     C                   ADD       1             Y3
168600970318     C                   MOVEL     WSGN          SG3(Y3)
168700970318     C                   END
168800980220      *
168900980219     C                   END
169000980220      *-------------------------------------------------
169100980220      *  Se devo inviare solo colli negativi ed ho IDD li carico in
169200980220      *  SG4 (nel caso non abbia ancora attivato flag FL1)
169300970619     C     §PTDET        IFEQ      'N'
169400970619     C     BLTDCM        ANDNE     0
169500970619     C     BLTFL1        ANDEQ     ' '
169600980220      *
169700970619     C     BLPCCA        IFEQ      '3'
169800970619     C     BLPCCA        OREQ      '5'
169900980220      *
170000970619     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
170100980219     C     §PUBS1        IFEQ      'E'
170200980219     C                   EXSR      CARSGE                                       EUROEXPRESS
170300980219     C                   ELSE
170400980219     C                   EXSR      CARSGN                                       BARTOLINI
170500980219     C                   END
170600980219      *
170700980219     C     WSGN          IFNE      *BLANKS
170800970619     C                   ADD       1             Y4
170900970619     C                   MOVEL     WSGN          SG4(Y4)
171000970619     C                   END
171100980219      *
171200980219     C                   END
171300970620     C                   END
171400980220      *
171500980220     C                   END                                                    -- 02 --
171600980220      *
171700050331      * Se è sempre richiesto il dettaglio Segnacolli sul Partner/Cliente ed
171800050331      * è una spedizione di RESO:
171900050331      * (poichè lo status di RESO viene inviata 2 volte da noi con 2 date differenti:
172000050331      *   la prima volta con la data della bolla di reso generata, la seconda con la
172100050331      *    data di consegna della bolla di reso. Vedi FNVAC00T)
172200050331      * ALLORA SOLO IN QUESTO CASO non si fleggano i segnacolli come trasmessi.
172300050331      *  in maniera da poterli sempre ritrasmettere al Partner/Cliente.
172400050331      * (Caso ARVATO che ragiona esclusivamente sui segnacolli e non sul rif.Sped.)
172500050331     C     §PTDET        ifEQ      'S'
172600050331     C     VACCCA        IFEQ      '2'
172700050331     C     VACCCA        OREQ      '6'
172800050331     c                   goto      salta_UPD
172900050331     c                   end
173000050331     c                   end
173100050331      *
173200980220      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
173300970304     C     BLTDCM        IFNE      0
173400970303     C                   MOVEL     'C'           BLTFTR
173500970303     C                   EXCEPT    AGGTRA
173600970304     C                   END
173700050331      *
173800050331     c     salta_UPD     tag
173900980220      *
174000980220      * Lettura successiva
174100960912     C     KBLT          READE     FNBLT01L                               31
174200980220     C                   END
174300980220      *
174400980220     C                   ENDSR
174500990604      *----------------------------------------------------------------
174600990604      *? COLCA - Routine caricamento colli da C.A.
174700990604      *----------------------------------------------------------------
174800990604     C     COLCA         BEGSR
174900990604      *
175000990604     C                   Z-ADD     *ZEROS        Y4                5 0
175100990604     C                   Z-ADD     *ZEROS        WCOLLI            5 0
175200040715      *
175300040715      * SOSTITUISCO LETTURA DEL FILE FNDCT02L CON LA RICERCA DELLE CA LEGATE ALL
175400040715      * SIA COME MAMMA CHE COME FIGLIA
175500040715     c                   clear                   fidn12ds
175600040715     c                   eval      i12aas = kAAS
175700040715     c                   eval      i12lnp = kLNP
175800040715     c                   eval      i12nrs = kNRS
175900040715     c                   eval      i12nsp = kNSP
176000040715     c                   eval      i12fel = 'E'
176100040715     c                   eval      i12fan = 'E'
176200040715     c                   eval      i12fch = 'E'
176300040715     c                   eval      i12fpt = 'E'
176400040715     c                   call      'FIDN12R'
176500040715     c                   parm                    fidn12ds
176600040715     c                   z-add     *zeros        ii                2 0
176700040715      **
176800040715      * se trovata almeno una CA
176900040715     c                   if        o12nca > 0
177000040715     c                   dow       ii <  o12nca
177100040715      **
177200040715     c                   add       1             ii
177300040715     c                   movea     skkey(ii)     dskey
177400040715     C                   z-add     dsaac         kaac
177500040715     C                   z-add     dsfil         kfil
177600040715     C                   z-add     dsnca         knca
177700040715     c     kdct          chain     FNDCT000
177800040715     c                   If        %found(fndct01l)
177900040715     C                   MOVEL     DCTFLO        DDCT01
178000040715     C     §DCTtrpt      ifeq      *BLANKS
178100990607      *
178200990607      *  Flaggo C.A. come trasmessa
178300020412     C                   MOVEL     'S'           §DCTtrpt
178400990607     C                   MOVEL     DDCT01        DCTFLO
178500990607     C                   EXCEPT    AGGDCT
178600990607      *
178700990604      *  Incremento numero colli
178800990604     C                   ADD       DCTNCN        WCOLLI
178900990604      *
179000990604      *  Controllo se devo caricare i segnacolli
179100990604     C     §PTDET        IFNE      'M'                                          -- 03 --
179200990604      *
179300990604      *  Leggo segnacolli C.A.: non annullati, e non chiusi
179400990604     C     KDCD          CHAIN     FNDCD000                           32
179500990607     C     *IN32         DOWEQ     '0'                                          -- 04 --
179600990604     C     DCDATB        IFEQ      *BLANKS                                      -- 05 --
179700990604     C     DCDDCH        ANDEQ     *ZEROS
179800990604      *
179900990604     C     §PUBS1        IFEQ      'E'
180000990604     C                   EXSR      CARSGE                                       EUROEXPRESS
180100990604     C                   ELSE
180200990604     C                   EXSR      CARSGN                                       BARTOLINI
180300990604     C                   ENDIF
180400990604      *
180500990604     C     WSGN          IFNE      *BLANKS
180600990604     C                   ADD       1             Y4
180700990604     C                   MOVEL     WSGN          SG4(Y4)
180800990604     C                   ENDIF
180900990604      *
181000990604     C                   ENDIF                                                  -- 05 --
181100990607     C     KDCD          READE     FNDCD000                               32
181200990604     C                   ENDDO                                                  -- 04 --
181300040715      *
181400040715     C                   ENDIF                                                  -- 03 --
181500040715      *****
181600040715     C                   endif
181700040715     C                   endif
181800040715      *
181900040715     c                   enddo
182000040715     c                   endif
182100040715      *
182200990607      * SCRIVO FLAT FILE
182300990607     C     §PTDET        IFEQ      'M'                                          -- 01 --
182400990607     C     Y4            OREQ      *ZEROS
182500990607     C                   Z-ADD     WCOLLI        DE7224
182600990607     C                   EXSR      WRT20
182700990607     C                   ELSE
182800990607      *
182900990607     C     Y4            IFNE      *ZEROS                                       -- 02 --
183000050427      *  Nel GID imposta il numero dei Colli Danneggiati
183100050427     C                   Z-ADD     y4            DE7224
183200990607     C                   Z-ADD     *ZEROS        WW
183300990607      *
183400990607     C                   DO        Y4            WE                             -- 03 --
183500990607      *  Scrivo ogni §PUEL2 elementi
183600990607     C     WW            IFEQ      §PUEL2                                       -- 04 --
183700990607     C                   MOVEL     '24'          DE4233
183800990607     C                   EXSR      WRT20
183900990607     C                   MOVEL     *BLANKS       EDSD20
184000990607     C                   Z-ADD     *ZEROS        WW
184100990607     C                   ENDIF                                                  -- 04 --
184200990607     C                   ADD       1             WW
184300990607     C                   MOVEL     SG4(WE)       D20(WW)
184400990607     C                   ENDDO                                                  -- 03 --
184500990607      *
184600990607      *  Se ci sono ancora dei colli scrittura nuovo record
184700990607     C     WW            IFNE      *ZEROS                                       -- 03 --
184800990607     C                   MOVEL     '24'          DE4233
184900990607     C                   EXSR      WRT20
185000990607     C                   MOVEL     *BLANKS       EDSD20
185100990607     C                   ENDIF                                                  -- 03 --
185200990607      *
185300990607     C                   ENDIF                                                  -- 02 --
185400990607     C                   ENDIF                                                  -- 01 --
185500990607      *
185600990604     C                   ENDSR
185700980220      *----------------------------------------------------------------
185800980220      *? WRTCOL - Routine scrittura colli
185900980220      *----------------------------------------------------------------
186000980220     C     WRTCOL        BEGSR
186100980220      *
186200980220      *  Scrivo i colli non consegnati
186300980220     C     Y2            IFGT      0
186400960912     C                   MOVE      Y2            DE7224
186500960912     C                   Z-ADD     0             WW                3 0
186600980220      *
186700980220     C                   DO        Y2            WE                3 0
186800980220      *
186900990607      *  Scrivo ogni §PUEL2 elementi
187000990607     C     WW            IFEQ      §PUEL2
187100990607     C                   MOVEL     '24'          DE4233
187200990607     C                   EXSR      WRT20
187300990607     C                   MOVEL     *BLANKS       EDSD20
187400990607     C                   Z-ADD     0             WW
187500990607     C                   ENDIF
187600990607     C                   ADD       1             WW
187700990607     C                   MOVEL     SG2(WE)       D20(WW)
187800990607     C                   ENDDO
187900990607      *
188000990607      *  Se ci sono ancora dei colli scrittura nuovo record
188100990607     C     WW            IFNE      *ZEROS
188200990607     C                   MOVEL     '24'          DE4233
188300990607     C                   EXSR      WRT20
188400990607     C                   MOVEL     *BLANKS       EDSD20
188500990607     C                   ENDIF
188600990607      *
188700990607     C                   ENDIF
188800980220      *
188900980220      *  Scrivo i colli consegnati
189000980220     C     Y1            IFGT      0                                            -- 01 --
189100980223      *
189200980220      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
189300980220      *  (conse. parziale --> ci sono colli non consegnati)
189400980220      *  (pratica IDD --> ci sono colli persi e colli consegnati)
189500980220     C     Y2            IFGT      0                                            -- 02 --
189600081216     C                   ADD       1             WPROG
189700960912     C                   MOVEL     WPROG         DA1490
189800960912     C                   MOVEL     '9999'        DA1312
189900960912     C                   CLEAR                   WDAT
190000960912     C                   MOVEL     EDSD00        WDAT
190100960912     C                   Z-ADD     §MSNUM        STAPRG
190200960912     C                   MOVEL     'SD00'        STATPR
190300960912     C                   MOVEL     WDAT          STADAT
190400120830      *
190500120830      *  Testata messaggio  GID-PCI
190600120830     c                   exsr      write_OUTPUT
190700110315      *
190800080422     c                   eval      Righe_Dett = 'S'
190900980223      *
191000980220      *  Consegna !!!!!!!
191100960912     C                   Z-ADD     1             X
191200980220     C     'CPA'         LOOKUP    C2A(X)                                 32       ?? CONSE.TOT
191300980220      *
191400980220     C     *IN32         IFEQ      '1'                                          -- 03 --
191500980220     C     §PULAB        IFEQ      'S'                                          -- 04 --
191600961022     C                   MOVEL     CEV(X)        DB9013
191700961022     C                   MOVEL     STS(X)        DB9011
191800980220     C                   ELSE                                                   -- 04 --
191900961022     C                   MOVEL     CDV(X)        DB9013
192000961022     C                   MOVEL     STV(X)        DB9011
192100980220     C                   END                                                    -- 04 --
192200960912     C                   MOVEL     '7  '         DB2005
192300960913     C                   MOVEL     BLPDCP        WDATHM
192400960912     C                   MOVE      VACHMC        WDATHM           12 0
192500960912     C                   MOVEL     WDATHM        DB2380
192600960912     C                   MOVEL     '203'         DB2379
192700980220     C                   END                                                    -- 03 --
192800980223      *
192900980223      *  Scrivo record evento CONSEGNA
193000960912     C                   MOVEL     EDSD05        WDAT
193100960912     C                   Z-ADD     §MSNUM        STAPRG
193200960912     C                   MOVEL     'SD05'        STATPR
193300960912     C                   MOVEL     WDAT          STADAT
193400120830      *
193500120830      *  Testata messaggio  STS-RFF-DTM
193600120830     c                   exsr      write_OUTPUT
193700110315      *
193800080422     c                   eval      Righe_Dett = 'S'
193900980223      *
194000980223      *  Pulisco SD20
194100960913     C                   CLEAR                   EDSD20
194200960913     C                   MOVEL     '99999'       DE1496
194300980220     C                   END                                                    -- 02 --
194400980223      *
194500980223      *  Scrivo D20 da SG1
194600960912     C                   MOVE      Y1            DE7224
194700960912     C                   Z-ADD     0             WW                3 0
194800980220     C                   DO        Y1            WE                3 0          -- 02 --
194900980223      *
195000990607      *  Scrivo ogni §PUEL2 elementi
195100990607     C     WW            IFEQ      §PUEL2
195200960913     C                   MOVEL     '24'          DE4233
195300960912     C                   EXSR      WRT20
195400990607     C                   MOVEL     *BLANKS       EDSD20
195500990607     C                   Z-ADD     *ZEROS        WW
195600990607     C                   ENDIF
195700990607     C                   ADD       1             WW
195800990607     C                   MOVEL     SG1(WE)       D20(WW)
195900990607     C                   ENDDO                                                  -- 02 --
196000980223      *
196100990607      *  Se ci sono ancora dei colli scrittura nuovo record
196200990607     C     WW            IFNE      *ZEROS                                       -- 02 --
196300990607     C                   MOVEL     '24'          DE4233
196400990607     C                   EXSR      WRT20
196500960912     C                   MOVEL     *BLANKS       EDSD20
196600990607     C                   ENDIF                                                  -- 02 --
196700980223      *
196800980220     C                   ELSE                                                   -- 01 --
196900980223      *
197000980223      *  Scrivo D20 da SG1
197100970619     C                   Z-ADD     Y1            DE7224
197200970619     C                   Z-ADD     0             WW                3 0
197300980220     C                   DO        Y1            WE                3 0          -- 02 --
197400990607      *
197500990607      *  Scrivo ogni §PUEL2 elementi
197600990607     C     WW            IFEQ      §PUEL2
197700990607     C                   MOVEL     '24'          DE4233
197800990607     C                   EXSR      WRT20
197900990607     C                   MOVEL     *BLANKS       EDSD20
198000990607     C                   Z-ADD     *ZEROS        WW
198100990607     C                   ENDIF
198200990607     C                   ADD       1             WW
198300990607     C                   MOVEL     SG1(WE)       D20(WW)
198400990607     C                   ENDDO                                                  -- 02 --
198500990607      *
198600990607      *  Se ci sono ancora dei colli scrittura nuovo record
198700990607     C     WW            IFNE      *ZEROS                                       -- 02 --
198800990607     C                   MOVEL     '24'          DE4233
198900990607     C                   EXSR      WRT20
199000990607     C                   MOVEL     *BLANKS       EDSD20
199100990607     C                   ENDIF                                                  -- 02 --
199200980223      *
199300980220     C                   END                                                    -- 01 --
199400980223      *
199500980223      *  Controllo se devo scrivere colli chiusi x IDD parziale
199600980220     C     Y3            IFGT      0                                            -- 01 --
199700980223      *
199800980223      *  Scrivo di nuovo SD00 per consegna parziale o per pratica IDD
199900980223      *  (conse.parziale --> ci sono colli non consegnati)
200000980223      *  (pratica IDD --> ci sono colli persi e colli consegnati)
200100980220     C     Y2            IFGT      0                                            -- 02 --
200200081216     C                   ADD       1             WPROG
200300970318     C                   MOVEL     WPROG         DA1490
200400970318     C                   MOVEL     '9999'        DA1312
200500970318     C                   CLEAR                   WDAT
200600970318     C                   MOVEL     EDSD00        WDAT
200700970318     C                   Z-ADD     §MSNUM        STAPRG
200800970318     C                   MOVEL     'SD00'        STATPR
200900970318     C                   MOVEL     WDAT          STADAT
201000120830      *
201100120830      *  Testata messaggio  GID-PCI
201200120830     c                   exsr      write_OUTPUT
201300110315      *
201400080422     c                   eval      Righe_Dett = 'S'
201500980223      *
201600980223      *  Consegna !!!!!!!
201700970318     C                   Z-ADD     1             X
201800970619     C     VACCCA        IFEQ      '7'
201900970617     C     'MAN'         LOOKUP    C2A(X)                                 32
202000990607     C                   ELSE
202100970617     C     'PID'         LOOKUP    C2A(X)                                 32
202200990607     C                   END
202300980220      *
202400990607     C     *IN32         IFEQ      '1'                                          -- 03 --
202500990607     C     §PULAB        IFEQ      'S'
202600970318     C                   MOVEL     CEV(X)        DB9013
202700970318     C                   MOVEL     STS(X)        DB9011
202800990607     C                   ELSE
202900970318     C                   MOVEL     CDV(X)        DB9013
203000970318     C                   MOVEL     STV(X)        DB9011
203100990607     C                   END
203200970318     C                   MOVEL     '7  '         DB2005
203300970318     C                   MOVEL     BLPDCP        WDATHM
203400970318     C                   MOVE      VACHMC        WDATHM           12 0
203500970318     C                   MOVEL     WDATHM        DB2380
203600970318     C                   MOVEL     '203'         DB2379
203700990607     C                   END                                                    -- 03 --
203800980223      *
203900980223      *  Scivo record evento CONSEGNA
204000970318     C                   MOVEL     EDSD05        WDAT
204100970318     C                   Z-ADD     §MSNUM        STAPRG
204200970318     C                   MOVEL     'SD05'        STATPR
204300970318     C                   MOVEL     WDAT          STADAT
204400120830      *
204500120830      *  Testata messaggio  STS-RFF-DTM
204600120830     c                   exsr      write_OUTPUT
204700110315      *
204800080422     c                   eval      Righe_Dett = 'S'
204900980223      *
205000980223      *  Pulisco SD20
205100970318     C                   CLEAR                   EDSD20
205200970318     C                   MOVEL     '99999'       DE1496
205300990607     C                   END                                                    -- 02 --
205400980223      *
205500980223      *  Scrivo D20 da SG3
205600970318     C                   MOVE      Y3            DE7224
205700970318     C                   Z-ADD     0             WW                3 0
205800990607     C                   DO        Y3            WE                3 0          -- 02 --
205900990607      *
206000990607      *  Scrivo ogni §PUEL2 elementi
206100990607     C     WW            IFEQ      §PUEL2
206200990607     C                   MOVEL     '24'          DE4233
206300990607     C                   EXSR      WRT20
206400990607     C                   MOVEL     *BLANKS       EDSD20
206500990607     C                   Z-ADD     *ZEROS        WW
206600990607     C                   ENDIF
206700990607     C                   ADD       1             WW
206800990607     C                   MOVEL     SG3(WE)       D20(WW)
206900990607     C                   ENDDO                                                  -- 02 --
207000990607      *
207100990607      *  Se ci sono ancora dei colli scrittura nuovo record
207200990607     C     WW            IFNE      *ZEROS                                       -- 02 --
207300990607     C                   MOVEL     '24'          DE4233
207400990607     C                   EXSR      WRT20
207500990607     C                   MOVEL     *BLANKS       EDSD20
207600990607     C                   ENDIF                                                  -- 02 --
207700980220      *
207800990607     C                   ENDIF                                                  -- 01 --
207900990607      *
208000980220      * Per CCA=5 o CCA=3 e  §PTDET='N' Y2, Y3 = 0 e Y4 > 0 imposto Y4 e SG4
208100990607     C     BLPCCA        IFEQ      '5'                                          -- 01 --
208200970619     C     BLPCCA        OREQ      '3'
208300980220      *
208400990607     C     §PTDET        IFEQ      'N'                                          -- 02 --
208500970619     C     Y2            ANDEQ     0
208600970619     C     Y3            ANDEQ     0
208700970619     C     Y4            ANDNE     0
208800970619     C                   Z-ADD     Y4            DE7224
208900970619     C                   Z-ADD     0             WW                3 0
209000990607     C                   DO        Y4            WE                3 0          -- 03 --
209100990607      *
209200990607      *  Scrivo ogni §PUEL2 elementi
209300990607     C     WW            IFEQ      §PUEL2
209400990607     C                   MOVEL     '24'          DE4233
209500990607     C                   EXSR      WRT20
209600990607     C                   MOVEL     *BLANKS       EDSD20
209700990607     C                   Z-ADD     *ZEROS        WW
209800990607     C                   ENDIF
209900990607     C                   ADD       1             WW
210000990607     C                   MOVEL     SG4(WE)       D20(WW)
210100990607     C                   ENDDO                                                  -- 03 --
210200990607      *
210300990607      *  Se ci sono ancora dei colli scrittura nuovo record
210400990607     C     WW            IFNE      *ZEROS                                       -- 03 --
210500990607     C                   MOVEL     '24'          DE4233
210600990607     C                   EXSR      WRT20
210700990607     C                   MOVEL     *BLANKS       EDSD20
210800990607     C                   ENDIF                                                  -- 03 --
210900990607      *
211000990607     C                   END                                                    -- 02 --
211100990607     C                   END                                                    -- 01 --
211200980223      *
211300960912     C                   ENDSR
211400980223      *----------------------------------------------------------------
211500980223      *? TUTTI - fleggo tutti i record di BLT come trasmessi
211600980223      *----------------------------------------------------------------
211700970304     C     TUTTI         BEGSR
211800980223      *
211900980223      *  Aggiorno come trasmessi i singoli colli
212000970303     C     KBLT          CHAIN     FNBLT01L                           31
212100970303     C     *IN31         DOWEQ     '0'
212200980223      *
212300980223      *  Considero solo i colli non trasmessi al cliente
212400970303     C     BLTFTR        IFNE      'C'
212500980223      *
212600980223      *  Fleggo segnacolli come trasmessi se consegnati
212700970303     C     BLTDCM        IFGT      0
212800980223      *
212900980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
213000970303     C                   MOVEL     'C'           BLTFTR
213100970303     C                   EXCEPT    AGGTRA
213200970303     C                   END
213300970304     C                   END
213400980223      * Lettura successiva
213500970303     C     KBLT          READE     FNBLT01L                               31
213600970303     C                   END
213700980223      *
213800970304     C                   ENDSR
213900980223      *----------------------------------------------------------------
214000980223      *? REDBLT - fleggo tutti i record di BLT come trasmessi
214100980223      *----------------------------------------------------------------
214200970304     C     REDBLT        BEGSR
214300980223      *
214400970304     C                   Z-ADD     0             WCOLCO            5 0
214500970304     C                   Z-ADD     0             WCOLID            5 0
214600980223      *
214700980223      *  Aggiorno come trasmessi i singoli colli
214800970304     C     KBLT          CHAIN     FNBLT01L                           31
214900970304     C     *IN31         DOWEQ     '0'
215000980223      *
215100980223      *  Considero solo i colli non trasmessi al cliente
215200970304     C     BLTFTR        IFNE      'C'
215300980223      *
215400980223      *  Fleggo segnacolli come trasmessi se consegnati
215500970304     C     BLTDCM        IFGT      0
215600970320     C     BLTFL1        OREQ      '5'
215700970617     C     BLTFL1        OREQ      '7'
215800970617     C     BLPCCA        OREQ      '3'
215900980223      *
216000980223      *  Se chiusi con pratica IDD aggiungo 1 al relativo nr.colli
216100980223      *  altrimenti CONTROLLO SE sono colli consegnati
216200970304     C     BLTFL1        IFEQ      '5'
216300970617     C     BLTFL1        OREQ      '7'
216400970617     C     BLPCCA        OREQ      '3'
216500970304     C                   ADD       1             WCOLID
216600970304     C                   ELSE
216700970304     C                   ADD       1             WCOLCO
216800970320     C                   END
216900980223      *
217000980223      * Aggiorno dettaglio segnacolli con flag trasmissione cliente
217100970304     C                   MOVEL     'C'           BLTFTR
217200970304     C                   EXCEPT    AGGTRA
217300970304     C                   END
217400970304     C                   END
217500980223      * Lettura successiva
217600970304     C     KBLT          READE     FNBLT01L                               31
217700970304     C                   END
217800980223      *
217900980223      * Se ci sono dei colli chiusi con pratica IDD scrivo TD20 per loro
218000970304     C     WCOLID        IFNE      0
218100970304     C                   MOVE      WCOLID        DE7224
218200970304     C                   EXSR      WRT20
218300980223      *
218400980223      * Se ci sono anche dei colli consegnati normalmente riscrivo SD00 e SD05
218500970304     C     WCOLCO        IFNE      0
218600081216     C                   ADD       1             WPROG
218700970304     C                   MOVEL     WPROG         DA1490
218800970304     C                   MOVEL     '9999'        DA1312
218900970304     C                   CLEAR                   WDAT
219000970304     C                   MOVEL     EDSD00        WDAT
219100970304     C                   Z-ADD     §MSNUM        STAPRG
219200970304     C                   MOVEL     'SD00'        STATPR
219300970304     C                   MOVEL     WDAT          STADAT
219400120830      *
219500120830      *  Testata messaggio  GID-PCI
219600120830     c                   exsr      write_OUTPUT
219700110315      *
219800080422     c                   eval      Righe_Dett = 'S'
219900980223      * consegna parziale
220000970304     C                   Z-ADD     1             X
220100970304     C     'CPA'         LOOKUP    C2A(X)                                 32     ???? CONSE.TOT
220200970304     C     *IN32         IFEQ      '1'
220300970304     C     §PULAB        IFEQ      'S'
220400970304     C                   MOVEL     CEV(X)        DB9013
220500970304     C                   MOVEL     STS(X)        DB9011
220600970304     C                   ELSE
220700970304     C                   MOVEL     CDV(X)        DB9013
220800970304     C                   MOVEL     STV(X)        DB9011
220900970304     C                   END
221000970304     C                   MOVEL     '7  '         DB2005
221100970304     C                   MOVEL     BLPDCP        WDATHM
221200970304     C                   MOVE      VACHMC        WDATHM           12 0
221300970304     C                   MOVEL     WDATHM        DB2380
221400970304     C                   MOVEL     '203'         DB2379
221500970304     C                   END
221600980223      *
221700980223      *  Scivo record evento CONSEGNA
221800970304     C                   MOVEL     EDSD05        WDAT
221900970304     C                   Z-ADD     §MSNUM        STAPRG
222000970304     C                   MOVEL     'SD05'        STATPR
222100970304     C                   MOVEL     WDAT          STADAT
222200120830      *
222300120830      *  Testata messaggio  STS-RFF-DTM
222400120830     c                   exsr      write_OUTPUT
222500110315      *
222600080422     c                   eval      Righe_Dett = 'S'
222700980223      *  Pulisco SD20
222800970304     C                   CLEAR                   EDSD20
222900970304     C                   MOVEL     '99999'       DE1496
223000970304     C                   END
223100970304     C                   END
223200980223      *
223300980223      * Scrivo colli consegnati regolarmente
223400970304     C     WCOLCO        IFNE      0
223500970304     C                   MOVE      WCOLCO        DE7224
223600970304     C                   EXSR      WRT20
223700970304     C                   END
223800980223      *
223900970320     C                   ENDSR
224000980220      *----------------------------------------------------------------
224100980220      *? CHKBLT - Controllo se ci sono colli da trasmettere
224200980220      *----------------------------------------------------------------
224300970320     C     CHKBLT        BEGSR
224400980220      *
224500980220      *  Aggiorno come trasmessi i singoli colli
224600970320     C                   MOVEL     'N'           WTRCOL            1
224700970320     C     KBLT          CHAIN     FNBLT01L                           31
224800980220      *
224900970320     C     *IN31         DOWEQ     '0'
225000970320     C     WTRCOL        ANDEQ     'N'
225100980220      *  Considero solo i colli non trasmessi al cliente
225200970320     C     BLTFTR        IFNE      'C'
225300980220      *  Fleggo segnacolli come trasmessi se consegnati
225400970320     C     BLTDCM        IFGT      0
225500970320     C     BLTFL1        OREQ      '5'
225600970320     C                   MOVEL     'S'           WTRCOL
225700970320     C                   END
225800970320     C                   END
225900980220      * Lettura successiva
226000970320     C     KBLT          READE     FNBLT01L                               31
226100970320     C                   END
226200980220      *
226300970320     C                   ENDSR
226400980223      *----------------------------------------------------------------
226500980223      *? WRT20 - scrivo record d20
226600980223      *----------------------------------------------------------------
226700970304     C     WRT20         BEGSR
226800980223      *  Scrivo record SD20
226900960912     C                   MOVEL     EDSD20        WDAT
227000960912     C                   MOVEL     'SD20'        STATPR
227100960912     C                   Z-ADD     §MSNUM        STAPRG
227200960912     C                   MOVEL     WDAT          STADAT
227300120830      *
227400120830      *  Testata messaggio  GID-PCI
227500120830     c                   exsr      write_OUTPUT
227600110315      *
227700080422     c                   eval      Righe_Dett = 'S'
227800980223      *
227900960912     C                   ENDSR
228000980223      *----------------------------------------------------------------
228100980223      *? CARSGE - caricamento segnacollo EUROEXPRESS
228200980223      *----------------------------------------------------------------
228300980219     C     CARSGE        BEGSR
228400980219      *
228500990607     C     VACCCA        IFEQ      'A'
228600990607     C                   Z-ADD     DCDFLS        KFLS
228700990607     C                   Z-ADD     DCDLNA        KLNA
228800990607     C                   Z-ADD     DCDNRS        KNRS
228900990607     C                   Z-ADD     DCDNSC        KNSC
229000990607     C                   ELSE
229100990607     C                   Z-ADD     BLTFLS        KFLS
229200990607     C                   Z-ADD     BLTLNA        KLNA
229300990607     C                   Z-ADD     BLTNRS        KNRS
229400990607     C                   Z-ADD     BLTNSC        KNSC
229500990607     C                   ENDIF
229600050120      *
229700050120     C                   MOVEL     'E'           KTRC
229800050120      *
229900050120      * Controlla se non è Euroexpress ma un cliente normale come Disk C
230000050120     C     kfls          chain     AZORG01L
230100050120     C                   clear                   OG143
230200050120     c                   if        %Found(AZORG01L)
230300050120     C                   movel     ORGDE3        OG143
230400050120     C     §OGntw        IFne      'EEX'
230500050120     C                   z-add     1             KKUT
230600050120     C                   movel     '1B'          KCOD
230700050120     C                   movel(p)  §ptCTM        KKEY
230800050120     C     KTAB2         chain     tabel00f
230900050120     c                   if        %Found(tabel00f)
231000050120     c                   movel     tbluni        ds1B
231100050120      * se si tratta di un Disk "C" non Euroexpress --> Cliente
231200050120     c                   if        §1BF12 <> 'N'
231300050120     C                   MOVEL     'C'           KTRC
231400050120     c                   end
231500050120     c                   end
231600050120     c                   end
231700050120     c                   end
231800050120      *
231900980219     C                   MOVEL     *BLANKS       WSGN
232000980219      *
232100051017     C     KARS          CHAIN     FIARS000                           33
232200051017     C  N33              MOVEL     ARSNOT        WSGN
232300980219      *
232400980219     C                   ENDSR
232500980223      *----------------------------------------------------------------
232600980223      *? CARSGN - caricamento segnacollo
232700980223      *----------------------------------------------------------------
232800980219     C     CARSGN        BEGSR
232900980219      *
233000980219      *  Se ha segnacollato il cliente carico in schiera i colli cons.
233100960912     C                   MOVEA     *BLANKS       C35
233200960912     C                   MOVEL     *BLANKS       WSGN             35
233300980219      *  Compongo lnp se la devo passare
233400960912     C     §PULP2        IFNE      0
233500960912     C                   Z-ADD     §PULP2        WA                3 0
233600961029     C                   MOVEL     BLPFLS        WLNP              3
233700960912     C                   MOVEA     WLNP          C35(WA)
233800960912     C                   END
233900980219      *  Compongo lna se la devo passare
234000960912     C     §PULA2        IFNE      0
234100990607     C                   Z-ADD     §PULA2        WA
234200960912     C                   MOVEL     BLPLNA        WLNA              3
234300960912     C                   MOVEA     WLNA          C35(WA)
234400960912     C                   END
234500980219      *  Compongo nrs se lo devo passare
234600960912     C     §PUNR2        IFNE      0
234700990607     C                   Z-ADD     §PUNR2        WA
234800960912     C                   MOVEL     BLPNRS        WNRS              2
234900960912     C                   MOVEA     WNRS          C35(WA)
235000960912     C                   END
235100980219      *  Compongo nsc se la devo passare
235200960912     C     §PUSG2        IFNE      0
235300990607     C                   Z-ADD     §PUSG2        WA
235400990607     C     VACCCA        IFEQ      'A'
235500990604     C                   MOVEL     DCDNSC        WNSC              7
235600990604     C                   ELSE
235700990604     C                   MOVEL     BLTNSC        WNSC
235800990604     C                   END
235900960912     C                   MOVEA     WNSC          C35(WA)
236000960912     C                   END
236100980219      *  Compongo znc se la devo passare
236200960912     C     §PUZN2        IFNE      0
236300990607     C                   Z-ADD     §PUZN2        WA
236400990607     C                   MOVEL     BLPZNC        WZNC              2
236500960912     C                   MOVEA     WZNC          C35(WA)
236600960912     C                   END
236700980219      *  metto C35 in WSGN
236800960912     C                   MOVEA     C35           WSGN
236900980227      *
237000960730     C                   ENDSR
237100960927     C*----------------------------------------------------------------
237200960927     C*? CARNUM - CARICAMENTO NUMERATORE
237300960927     C*----------------------------------------------------------------
237400960927     C     CARNUM        BEGSR
237500960927     C*
237600960927     C                   Z-ADD     0             WPROG             4 0
237700960927     C                   Z-ADD     0             X                 3 0
237800960927     C                   Z-ADD     1             §MSNUM
237900960927     C                   MOVEL     'MS'          KCOD1
238000960927     C                   MOVEL     *BLANKS       KKEY1
238100960927     C                   MOVEL     '784'         KKEY1
238200960927     C     KTABE         CHAIN     EDTAB01L                           30
238300960927     C     *IN30         IFEQ      '0'
238400960927     C     TABFLG        ANDEQ     *BLANKS
238500020412     C                   MOVEL     TABUNI        EDIDSMS
238600960927     C                   ADD       1             §MSNUM
238700061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
238800061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
238900061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
239000061219     c                   move      §MSNUM        campo_4n          4 0
239100061219     c                   if        campo_4n = 0
239200061219     c                   add       1             §MSNUM
239300061219     c                   end
239400960927     C                   Z-ADD     WOGGI         §MSDAT
239500020412     C                   MOVEL     EDIDSMS       TABUNI
239600960927     C                   UPDATE    EDTAB000
239700960927     C                   END
239800980223      *
239900960927     C                   ENDSR
240000980223      *----------------------------------------------------------------
240100980223      *? *INZSR - OPERAZIONI INIZIALI
240200980223      *----------------------------------------------------------------
240300960528     C     *INZSR        BEGSR
240400080418      *
240500080418      *?  Deve leggere il nuovo TIVGD x generare EDI e fare il Download...  ?
240600080418     C     *ENTRY        PLIST
240700080418     C                   PARM                    CLIENTE           8            "0xxxyyyy"
240800080418     C                   PARM                    Tipo_File         2            "VC"
240900080418     C                   PARM                    Tipo_Scarico      2            "EW"
241000080418     C                   PARM                    Righe_Dett        1            "N"
241100110630     C                   PARM                    Esito             1            ritorno errore
241200110601      *
241300110630      * reset campo compattazione x Download file
241400110630     C                   eval      esito = '0'
241500110601     c                   eval      Compatta_80 = 'S'
241600080418     c                   eval      Righe_Dett = 'N'
241700080418      *
241800980223      * Definisco chiavi di accesso
241900080418     C     KEY_VGD01     KLIST
242000080418     C                   KFLD                    Tipo_File                      "VC"
242100080418     C                   KFLD                    CLIENTE                        "0xxxyyyy"
242200080418     C                   KFLD                    Tipo_Scarico                   "EW"
242300080418      *
242400960726     C     KTABE         KLIST
242500960726     C                   KFLD                    KCOD1
242600960726     C                   KFLD                    KKEY1
242700960528     C     KBLP          KLIST
242800960528     C                   KFLD                    KAAS
242900960528     C                   KFLD                    KLNP
243000960528     C                   KFLD                    KNRS
243100960528     C                   KFLD                    KNSP
243200990604     C     KDCD          KLIST
243300990604     C                   KFLD                    DCTAAC
243400990604     C                   KFLD                    DCTFIL
243500990604     C                   KFLD                    DCTNCA
243600960528     C     KGCP          KLIST
243700960528     C                   KFLD                    KAAS
243800960528     C                   KFLD                    KLNP
243900960528     C                   KFLD                    KNRS
244000960528     C                   KFLD                    KNSP
244100960528     C                   KFLD                    KFRG
244200050922     C     KGCp_1        KLIST
244300050922     C                   KFLD                    KAAS
244400050922     C                   KFLD                    KLNP
244500050922     C                   KFLD                    KNRS
244600050922     C                   KFLD                    KNSP
244700961204     C     KGNP          KLIST
244800961204     C                   KFLD                    KAGC
244900961204     C                   KFLD                    KFGC
245000961204     C                   KFLD                    KNGC
245100961204     C                   KFLD                    KFRG
245200961204     C                   KFLD                    KFAS
245300960528     C     KBLT          KLIST
245400960528     C                   KFLD                    KAAS
245500960528     C                   KFLD                    KLNP
245600960528     C                   KFLD                    KNRS
245700960528     C                   KFLD                    KNSP
245800970319     C     KLBL1         KLIST
245900970319     C                   KFLD                    KAAN
246000970319     C                   KFLD                    KLPN
246100970319     C                   KFLD                    KNRN
246200970319     C                   KFLD                    KNSN
246300970319     C     KLBL2         KLIST
246400970319     C                   KFLD                    KAAP
246500970319     C                   KFLD                    KLPP
246600970319     C                   KFLD                    KNRP
246700970319     C                   KFLD                    KNSP
246800060213     C     Kar4          KLIST
246900960528     C                   KFLD                    KAAS
247000960528     C                   KFLD                    KLNP
247100960528     C                   KFLD                    KNRS
247200960528     C                   KFLD                    KNSP
247300960528     C                   KFLD                    KTRC
247400960628     C     KRDE          KLIST
247500960628     C                   KFLD                    KAAS
247600960628     C                   KFLD                    KLNP
247700960628     C                   KFLD                    KNRS
247800960628     C                   KFLD                    KNSP
247900960628     C                   KFLD                    KNMC
248000970319     C                   KFLD                    KNRP1
248100961029     C     KTAB1         KLIST
248200961029     C                   KFLD                    KKUT
248300961029     C                   KFLD                    KCOD
248400970319     C     KTAB2         KLIST
248500970319     C                   KFLD                    KKUT
248600970319     C                   KFLD                    KCOD
248700970319     C                   KFLD                    KKEY
248800970319     C     KEVB          KLIST
248900970319     C                   KFLD                    KAA2
249000970319     C                   KFLD                    KLNP
249100970319     C                   KFLD                    KNRS
249200970319     C                   KFLD                    KNSP
249300970319     C                   KFLD                    KCEV
249400980204     C     KEVB1         KLIST
249500980204     C                   KFLD                    KAA2
249600980204     C                   KFLD                    KLNP
249700980204     C                   KFLD                    KNRS
249800980204     C                   KFLD                    KNSP
249900980204     C                   KFLD                    KDTV
250000980204     C                   KFLD                    KORV
250100980204     C     KEVB2         KLIST
250200980204     C                   KFLD                    KAA2
250300980204     C                   KFLD                    KLNP
250400980204     C                   KFLD                    KNRS
250500980204     C                   KFLD                    KNSP
250600140910     C     KParzial      KLIST
250700140910     C                   KFLD                    KAAS
250800140910     C                   KFLD                    KLNP
250900140910     C                   KFLD                    KNRS
251000140910     C                   KFLD                    KNSP
251100140910     C                   KFLD                    KCEV_P            3
251200051017     C     KARS          KLIST
251300980219     C                   KFLD                    KFLS
251400980219     C                   KFLD                    KLNA
251500980219     C                   KFLD                    KNRS
251600980219     C                   KFLD                    KNSC
251700980219     C                   KFLD                    KTRC
251800040607     C     KARF          klist
251900040607     C                   kfld                    KAAS
252000040607     C                   kfld                    KLNP
252100040607     C                   kfld                    KNRS
252200040607     C                   kfld                    KNSP
252300040715
252400040715     C     Kdct          klist
252500040715     C                   KFLD                    kaac
252600040715     C                   KFLD                    kfil
252700040715     C                   KFLD                    knca
252800040715      *
252900980223      * Definizione campi di work
253000980219     C     *LIKE         DEFINE    BLTNSC        KNSC
253100980219     C     *LIKE         DEFINE    BLTLNA        KLNA
253200051017     C     *LIKE         DEFINE    ARSFLS        KFLS
253300980219     C     *LIKE         DEFINE    TBLKUT        KKUT
253400961029     C     *LIKE         DEFINE    TBLCOD        KCOD
253500970319     C     *LIKE         DEFINE    TBLKEY        KKEY
253600961029     C     *LIKE         DEFINE    TABCOD        KCOD1
253700960726     C     *LIKE         DEFINE    TABKEY        KKEY1
253800960528     C     *LIKE         DEFINE    BLPAAS        KAAS
253900960528     C     *LIKE         DEFINE    BLPLNP        KLNP
254000960528     C     *LIKE         DEFINE    BLPNRS        KNRS
254100960528     C     *LIKE         DEFINE    BLPNSP        KNSP
254200060213     C     *LIKE         DEFINE    ar4TRC        KTRC
254300960528     C     *LIKE         DEFINE    GCPFRG        KFRG
254400960628     C     *LIKE         DEFINE    RDENMC        KNMC
254500970319     C     *LIKE         DEFINE    RDENRP        KNRP1
254600961204     C     *LIKE         DEFINE    GNPAGC        KAGC
254700961204     C     *LIKE         DEFINE    GNPFGC        KFGC
254800961204     C     *LIKE         DEFINE    GNPNGC        KNGC
254900961204     C     *LIKE         DEFINE    GNPFAS        KFAS
255000970319     C     *LIKE         DEFINE    LBLAAN        KAAN
255100970319     C     *LIKE         DEFINE    LBLLPN        KLPN
255200970319     C     *LIKE         DEFINE    LBLNRN        KNRN
255300970319     C     *LIKE         DEFINE    LBLNSN        KNSN
255400970319     C     *LIKE         DEFINE    LBLAAP        KAAP
255500970319     C     *LIKE         DEFINE    LBLLPP        KLPP
255600970319     C     *LIKE         DEFINE    LBLNRP        KNRP
255700970319     C     *LIKE         DEFINE    EVBAAS        KAA2
255800970319     C     *LIKE         DEFINE    EVBCEV        KCEV
255900980204     C     *LIKE         DEFINE    EVBDTV        KDTV
256000980204     C     *LIKE         DEFINE    EVBORV        KORV
256100980223      *
256200980223      * Recupero codice AS
256300960528     C                   Z-ADD     1             CODUT             1 0
256400960705     C                   CALL      'X§PARUT'
256500020412     C                   PARM                    UT§DSE0F
256600960528     C                   MOVEL     REC80         CNCR80
256700980223      *
256800980223      * Recupero data e ora x messaggio
256900960528     C                   TIME                    WHHDAT           14 0
257000960708     C                   MOVE      WHHDAT        G02DAT
257100960528     C                   MOVE      WHHDAT        DATA              6 0
257200960528     C                   MOVEL     *BLANK        G02ERR
257300960528     C                   CALL      'XSRDA8'
257400960528     C                   PARM                    WLBDA8
257500960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
257600960708     C                   MOVEL     WHHDAT        WHHMM             4 0
257700960528     C                   MOVE      WHHDAT        WANNO             2 0
257800100304     C                   Z-ADD     woggi         WOGGIymd          6 0           UDATE
257900980629      * ---------------------------------------------
258000980629      *  CARICO TABELLE
258100980629      * ---------------------------------------------
258200980223      * Caricamento Tabella eventi con IDD
258300050909      *    o tabella Giacenze/Lasciati Avviso
258400970319     C                   Z-ADD     0             X
258500050909     C                   Z-ADD     0             Xj                3 0
258600050922     C                   Z-ADD     0             Xa                3 0
258700970319     C                   Z-ADD     1             KKUT
258800970319     C                   MOVEL     '2A'          KCOD
258900970319     C                   MOVEA     *BLANKS       IDD
259000050922     C                   MOVEA     *BLANKS       GiaC
259100050926     C                   MOVEA     *BLANKS       LAvv
259200970319     C     KTAB1         CHAIN     TABEL00F                           30
259300970319     C     *IN30         DOWEQ     '0'
259400970319     C     TBLFLG        IFEQ      *BLANKS
259500970319     C                   MOVEL     TBLUNI        DS2A
259600050909      * con IDD
259700970319     C     §2AIDD        IFNE      *BLANKS
259800970319     C                   ADD       1             X
259900970319     C                   MOVEL     TBLKEY        IDD(X)
260000970319     C                   END
260100050909      *
260200050922      * Cod. Giacenza
260300050926     C                   if        §2AFTC = 'G'
260400050909     C                   add       1             Xj
260500050922     C                   MOVEL     TBLKEY        GiaC(Xj)
260600050909     C                   end
260700050922      * Lasciato Avviso
260800050926     C                   if        §2AFTC = 'A'
260900050922     C                   add       1             Xa
261000050926     C                   MOVEL     TBLKEY        LAvv(Xa)
261100050922     C                   end
261200050922      *
261300970319     C                   END
261400970319     C     KTAB1         READE     TABEL00F                               30
261500970319     C                   END
261600030926      *
261700030926     ** carico la schiera dei P.O presenti sull AS
261800030926     c                   clear                   tibs56ds
261900030926     c                   eval      i56ppo = simfel
262000030926     c                   eval      i56mod = 'POS'
262100030926     c                   call      'TIBS56R'
262200030926     c                   parm                    tibs56ds
262300030926     C                   MOVEA     SKI           Pou
262400980629      *
262500980223      * Caricamento Tabella Partner esteri
262600960528     C                   Z-ADD     0             X                 3 0
262700960528     C                   MOVEL     'PT'          TABCOD
262800960528     C     TABCOD        CHAIN     EDTAB01L                           30
262900960528     C     *IN30         DOWEQ     '0'
263000960528     C     TABFLG        IFEQ      *BLANKS
263100960528     C                   ADD       1             X
263200960528     C                   MOVEL     TABKEY        CPT(X)
263300961023     C                   MOVEL     TABKEY        CPU(X)
263400961021     C                   MOVE      '    '        CPT(X)
263500960528     C                   MOVEL     TABUNI        KSC(X)
263600960708     C                   MOVEL     TABUNI        DPT(X)
263700960528     C                   END
263800960528     C     TABCOD        READE     EDTAB01L                               30
263900960528     C                   END
264000980223      *
264100091117      * Deve comunque controllare che i codici caricati dalla PT
264200091117      *  non abbiano un altro codice di riferimento sulla 3K con cui
264300091117      *  impostare il membro.
264400091117     c                   do        x             xx
264500091117     C                   Z-ADD     1             KKUT
264600091117     C                   MOVEL     '3K'          KCOD
264700120413      *
264800120413     C                   movel(p)  KSC(xx)       KKEY
264900091117     C     KTAB2         chain     tabel00f
265000091117     c                   if        %Found(tabel00f)
265100091117      *
265200091117     c                   movel     tbluni        ds3K
265300091117     c                   if        §3Kcks <> KSC(xx)
265400091117      *  sostituisce il codice con quello del membro
265500091117     c                   move      §3Kcks        Ksc(xx)
265600091117      *
265700091117     c                   end
265800091117     c                   end
265900091117     C                   END
266000980629      *
266100980629      *  In base al codice del PARTNER reperisco identificativo
266200080418     C                   MOVE      Cliente       WCCM              7 0
266300020221     C                   Z-ADD     1             XX                3 0
266400980629     C     WCCM          LOOKUP    KSC(XX)                                32
266500020916      *
266600070503      *?  Se non trova la "PT" non deve proseguire   ?
266700070503     C     *IN32         IFEQ      '0'
266800070503     c                   move      'S'           wFINE
266900070503     c                   leaveSR
267000070503     c                   end
267100070503      *
267200980629     C     *IN32         IFEQ      '1'
267300980629      *
267400020412     C                   MOVEL     DPT(XX)       EDIDSPT
267500980629      *
267600980629     C                   MOVEL     'PU'          KCOD1
267700980707     C                   MOVEL     CPU(XX)       KKEY1
267800980707     C     KTABE         CHAIN     EDTAB01L                           30
267900980629      *
268000980629     C     *IN30         IFEQ      '0'
268100980629     C     TABFLG        ANDEQ     *BLANKS
268200020412     C                   MOVEL     TABUNI        EDIDSPU
268300980629     C                   END
268400110318      *>>>>>>>>>>
268500110318      *   Con quale traduttore deve essere scaricato e scritto il File ????
268600110318     C                   MOVEL     'PZ'          KCOD1
268700110318     C                   MOVEL     CPU(XX)       KKEY1
268800110318     C     KTABE         CHAIN     EDTAB01L
268900110318     C                   if        %Found(EDTAB01L) and tabflg = *blank
269000110318     C                   MOVEL     TABUNI        EDIDSPz
269100110405      *
269200110405      * per scrivere il file EDIFTSTA in QTEMP altrimenti lo deve lasciare chiuso
269300110405     c                   if        §PZPGMTRSS = 'INTESA'
269400110405     C                   open      EDIFTSTA
269500110405     c                   end
269600110405      *
269700110318     C                   END
269800140221      *>>>>>>>>>>
269900140221      *   Controlla la presenza della tabella NUOVA dal "PS" nata il 21/2/2014
270000140221      *    per POTER NON generare lo stato di APPUNTAMENTO/SUPERMERCATO durante la
270100140221      *   generazione del MIC o della CONSEGNA
270200140221     C                   MOVEL     'PS'          KCOD1
270300140221     C                   MOVEL     CPU(XX)       KKEY1
270400140221     C                   clear                   EDIDSPs
270500140221     C                   eval      Invio_Appuntamento_Supermercato = *Blank
270600140416     C                   eval      Invio_RFF_CU = *Blank
270700140221     C     KTABE         CHAIN     EDTAB01L
270800140221     C                   if        %Found(EDTAB01L) and tabflg = *blank
270900140221     C                   MOVEL     TABUNI        EDIDSPs
271000140221     C                   eval      Invio_Appuntamento_Supermercato = §psAPP
271100140416     C                   eval      Invio_RFF_CU = §psRCU
271200160706     C                   eval      ordinamSTD_EDIFACT_FTX_NAD = §psFTXNAD
271300140221     C                   END
271400110318      *>>>>>>>>>>
271500980629      * Caricamento Tabella codici eventi. Se richesta Lista Labarta e codici
271600980629      *   Labarta sono Blanks non carico rcd, lo stesso per Lista VGL.
271700980629     C                   Z-ADD     0             X                 3 0
271800980629     C                   MOVEL     '2A'          TABCOD
271900980629      *
272000980629     C     TABCOD        CHAIN     EDTAB01L                           30
272100980629     C     *IN30         DOWEQ     '0'
272200020412     C                   MOVEL     TABUNI        EDIDS2A
272300980629      *
272400980629     C                   SELECT
272500980629      *
272600980629     C     TABFLG        WHENNE    *BLANKS
272700980629      *
272800980629     C     §PULAB        WHENEQ    'S'
272900980629     C     §2ASTS        ANDEQ     *BLANKS
273000980629     C     §2ACEV        ANDEQ     *BLANKS
273100980629      *
273200980629     C     §PULAB        WHENEQ    *BLANKS
273300980629     C     §2ASTV        ANDEQ     *BLANKS
273400980629     C     §2ACDV        ANDEQ     *BLANKS
273500980629      *
273600980629     C                   OTHER
273700980629     C                   ADD       1             X
273800980629     C                   MOVEL     TABKEY        C2A(X)
273900980629     C                   MOVEL     §2ACEV        CEV(X)
274000980629     C                   MOVEL     §2ASTS        STS(X)
274100980629     C                   MOVEL     §2ACDV        CDV(X)
274200980629     C                   MOVEL     §2ASTV        STV(X)
274300980629     C                   ENDSL
274400980629      *
274500980629     C     TABCOD        READE     EDTAB01L                               30
274600980629     C                   END
274700980629      *
274800980629     C                   END
274900980629      *---------------------------------------
275000980629      *
275100980223      * Caricamento NUMERATORE
275200960927     C                   EXSR      CARNUM
275300980223      * finta read x IFTSTA
275400960528     C   01
275500960528     CANN01              READ      EDIFTSTA                               01
275600980223      *
275700980223      * Controllo se esiste EDRDE
275800980126     C                   Z-ADD     45            LUNG             15 5
275900980126     C                   MOVEL     *BLANKS       COMMAN
276000980126     C                   MOVEA     CMD(1)        COMMAN           80
276100980126     C                   CALL      'QCMDEXC'                            21
276200980126     C                   PARM                    COMMAN
276300980126     C                   PARM                    LUNG
276400980126     C     *IN21         IFEQ      '0'
276500980126     C                   OPEN      EDRDE01L
276600980126     C                   MOVEL     'S'           WTRRDE            1
276700980126     C                   END
276800980223      *
276900960528     C                   ENDSR
277000110316      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
277100120830      *  Scrive l'OUTPUT da inviare
277200110316      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
277300120830     C     write_OUTPUT  BEGSR
277400110316      *
277500120830     c                   if        §PZPGMTRSS = 'INTESA'
277600120830     C                   WRITE     IFTSTA00
277700120830     c                   elseIf    §PZPGMTRSS = 'STA94A'
277800120830     c                   exsr      Exit_segmenti
277900120830     c                   elseIf    §PZPGMTRSS = 'GELSTA'
278000120830     c                   exsr      Exit_FileGEL
278100131003     c                   elseIf    §PZPGMTRSS = 'AMZSTA'
278200131003     c                   exsr      Exit_FileAMZ
278300120830     C                   EnD
278400120830      *
278500120830     C                   ENDSR
278600120830      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
278700120830      *  Scrittura dei segmenti IFTSTA D94A
278800120830      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
278900120830     C     Exit_Segmenti BEGSR
279000120830      *
279100110318     c                   If        §PZPGMTRSS = 'STA94A'
279200110318      *
279300110318     c                   eval      Tipo_DS = STATPR
279400110318     c                   eval      Suffisso   = §PUSIG
279500110601     c                   eval      Compatta_80 = §pucompat
279600110322     c                   clear                   DsGenerica
279700110322     c                   eval      DsGenerica = STADAT
279800110322     c                   call      'TRTCT81R1'
279900110318     c                   PARM                    CLIENTE
280000110318     c                   PARM                    Suffisso          2
280100110318     c                   PARM                    Tipo_DS           4
280200110318     c                   PARM                    DsGenerica     1024
280300110322     C                   PARM                    Numero_Righe      5 0
280400110601     c                   PARM                    Compatta_80       1
280500110617     C                   PARM                    Progressivo      10
280600110621     c                   PARM                    esito             1
280700110630     C
280800110630     C                   if        esito = 'E'
280900110630     c                   exsr      Invio_Mail
281000110630     c                   end
281100110318      *
281200110318     c                   end
281300120830      *
281400120830     C                   ENDSR
281500120830     c*==================================================================*
281600120830      * Manda un File a tracciato NETEXPRESS EUROPE
281700120830     c*==================================================================*
281800120830     c     Exit_FileGEL  begsr
281900120830      *
282000120830     c                   clear                   DsGenerica
282100120830     c                   eval      Tipo_DS = STATPR
282200120830     c                   eval      DsGenerica = STADAT
282300120905     c                   call      'TRTCT81GR'
282400120830     c                   PARM                    CLIENTE
282500120830     c                   PARM                    Tipo_DS           4
282600120830     c                   PARM                    DsGenerica     1024
282700120830     C                   PARM                    Numero_Righe      5 0
282800120830     C                   PARM                    Progressivo      10
282900120830     c                   PARM                    esito             1
283000120830     C
283100120830     C                   if        esito = 'E'
283200120830     c                   exsr      Invio_Mail
283300120830     c                   end
283400110316      *
283500110316     C                   ENDSR
283600131003     c*==================================================================*
283700131003      * Manda un File a tracciato IFTSTA Particolarissimo di AMAZON
283800131003     c*==================================================================*
283900131003     c     Exit_FileAMZ  begsr
284000131003      *
284100131003     c                   clear                   DsGenerica
284200131003     c                   eval      Tipo_DS = STATPR
284300131003     c                   eval      DsGenerica = STADAT
284400131003     c                   call      'TRTCT81AR'
284500131003     c                   PARM                    CLIENTE
284600131003     c                   PARM                    Tipo_DS           4
284700131003     c                   PARM                    DsGenerica     1024
284800131003     C                   PARM                    Numero_Righe      5 0
284900131003     C                   PARM                    Progressivo      10
285000131003     c                   PARM                    esito             1
285100131003     C
285200131003     C                   if        esito = 'E'
285300131003     c                   exsr      Invio_Mail
285400131003     c                   end
285500131003      *
285600131003     C                   ENDSR
285700110630     c*==================================================================*
285800110630      * Manda un Msg x E-mail
285900110630     c*==================================================================*
286000110630     c     Invio_Mail    begsr
286100110630      *
286200110630???? C                   EVAL      Oggetto ='Problemi DOWNLD STATI EDI !!'
286300110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione EDI -
286400110630     C                             STATI NON tradotto x invio su DOWNLD. -
286500110630     C                             Controllare il Download del Cliente. << '+
286600110630     c                             CLIENTE + ' >>'
286700110630      *
286800110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
286900110630     C* Inizializzo variabili
287000110630     C                   movel     *blanks       wrkEml          253
287100110630     C                   movel     *blanks       wrkMsg         5000
287200110630     C                   movel     *blanks       wrkOgg           44
287300110630     C* Valorizzo i campi della e-m@ail - indirizzo
287400110630     C                   eval      wrkEml = CED_BART
287500110630     C                   eval      wrkOgg = Oggetto
287600110630     C                   eval      wrkMsg = Messaggio
287700110630     C                   call(e)   'TRTCT00R2'
287800110630     C                   parm                    wrkEml
287900110630     C                   parm                    wrkOgg
288000110630     C                   parm                    wrkMsg
288100110630     C*
288200110630     C                   ENDSR
288300110630      * ?------------------------------------------------------------------ */
288400110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
288500110630      * ?------------------------------------------------------------------ */
288600110630     C     *pssr         BEGSR
288700110630     C
288800110630     C                   eval      esito = 'E'
288900110630     c                   exsr      Invio_Mail
289000110630     C
289100110630     C                   ENDSR     '*CANCL'
289200110630     C
289300110316      *-----------------------------------------------------*
289400980223      *   EXCPT   x aggiornamento flag trasmissione BLT     *
289500980223      *-----------------------------------------------------*
289600970303     OFNBLT000  E            AGGTRA
289700970303     O                       BLTFTR
289800990607      *-----------------------------------------------------*
289900990607      *   EXCPT   x aggiornamento flag trasmissione DCT     *
290000990607      *-----------------------------------------------------*
290100990607     OFNDCT000  E            AGGDCT
290200990607     O                       DCTFLO
290300980126**
290400980126CHKOBJ OBJ(EDRDE01L) OBJTYPE(*FILE)
