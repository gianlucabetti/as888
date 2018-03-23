000100110407      *   ATTENZIONE                                                        *
000200110407      *                                                                     *
000300110407      *       C O M P I L A R E    con TRGRLS   *CURRENT                    *
000400110407      *                                                                     *
000500930128      ***********************************************************************
000600961002      *   RITORNO DATE ARRIVO SPEDIZIONI              ESTERO                *
000700930128      ***********************************************************************
000800930128     H DECEDIT('0,') DATEDIT(*DMY.)
000900960528     F*---------------------------------------------------------------------*
001000960528     F*  DATA BASE                                                          *
001100960528     F*---------------------------------------------------------------------*
001200961010     FEDSTAWRK  UF   E           K DISK
001300960528     F*---------
001400110407     FEDSTA00F  UF A E             DISK    extDESC('EDIFTSTA')
001500961002     F*---------
001600961002     FEDTAB01L  UF   E           K DISK
001700120413     F*---------
001800120413     Ftabel00f  IF   E           K DISK
001900980330     F*---------
002000051017     FFIARS01L  IF   E           K DISK
002100140416     F*---------
002200140416     FFNBLP01L  IF   E           K DISK
002300960528     D*---------------------------------------------------------------------*
002400960528     D* Schiere
002500960528     D*---------------------------------------------------------------------*
002600960528     D* Schiera per scrittura dati località record ST10
002700960528     D* Schiera per scrittura testi liberi record ST15
002800960528     D* Schiera per scrittura testi liberi record SD15
002900960528     D* Schiera per reperimento nr.seganacollo
003000961010     D SGN             S             35    DIM(500)                             Sgn.Sped..
003100960528     D* Tabella codici eventi
003200960528     D CEV             S             35    DIM(200)
003300960528     D C2A             S              3    DIM(200)
003400960612     D STS             S              3    DIM(200)
003500960821     D* TESTO LIBERO
003600960821     D FTX             S             70    DIM(1) CTDATA PERRCD(1)
003700960912     D* composizione schiera 35 chr x segnacollo
003800960912     D C35             S              1    DIM(35)
003900140416     D Invio_RFF_CU...
004000140416     D                 s              1a
004100140417     D trovato_blp...
004200140417     D                 s              1a
004300960528     D*---------------------------------------------------------------------*
004400960528     D* DS
004500960528     D*---------------------------------------------------------------------*
004600960528     D* .. per scompozione dati da spedire a seconda del tipo record
004700960528     D EDST00        E DS                  EXTNAME(EDST00DS)
004800960708     D EDST05        E DS                  EXTNAME(EDST05DS)
004900960528     D EDST10        E DS                  EXTNAME(EDST10DS)
005000960528     D  T10                   48    103
005100960528     D                                     DIM(2)
005200960528     D EDST15        E DS                  EXTNAME(EDST15DS)
005300960528     D  T15                   13    362
005400960528     D                                     DIM(5)
005500960528     D EDSD00        E DS                  EXTNAME(EDSD00DS)
005600960528     D EDSD05        E DS                  EXTNAME(EDSD05DS)
005700960528     D EDSD10        E DS                  EXTNAME(EDSD10DS)
005800960528     D EDSD15        E DS                  EXTNAME(EDSD15DS)
005900960528     D  D15                   13    362
006000960528     D                                     DIM(5)
006100960528     D EDSD20        E DS                  EXTNAME(EDSD20DS)
006200960730     D  D20                   33    732
006300960730     D                                     DIM(20)
006400051202      **
006500960528     D*  Ds per scrittura dati località
006600960528     D WST10           DS
006700960528     D  WTPL                   1      3
006800960528     D  WLOC                   4     28
006900960528     D*---------------------------------------------------------------------*
007000110407     D* .. per reperimento tipo record di EDSTA00F da scrivere
007100960528     D*                    .. 5 - 8  : tipo record
007200040218     D WDAT            DS          1950
007300960729     D  STAPRG                 1      4  0
007400960726     D  STATPR                 5      8
007500960528     D*---------------------------------------------------------------------*
007600960528     D KPJBA         E DS
007700960528     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
007800960528     D CNCR80        E DS
007900120413     D DS3K          E DS
008000961016     D DSPT          E DS                  EXTNAME(EDIDSPT)
008100960912     D DSPU          E DS                  EXTNAME(EDIDSPU)
008200110330     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
008300110330     D DSPW          E DS                  EXTNAME(EDIDSPW)
008400110330     D DSPV          E DS                  EXTNAME(EDIDSPV)
008500140416     D DSPS          E DS                  EXTNAME(EDIDSPS)
008600960726     D DSMS          E DS                  EXTNAME(EDIDSMS)
008700961010     D TRTC87        E DS                  EXTNAME(TRTC87DS)
008800110330     d Righe_Dett      s              1
008900960528     D*  DS x scomposizione segnacollo
009000960528     D DSSGN           DS
009100960528     D  SGNLNP                 1      3  0
009200960528     D  SGNLNA                 4      6  0
009300960528     D  SGNNRS                 7      8  0
009400960528     D  SGNNSC                 9     15  0
009500960829     D  SGNZNC                16     17  0
009600961010     D*  DS x rottura livello spedizione - codice errore
009700961010     D SPEERR          DS
009800040305     D  CNI_STA                3     16  0
009900961010     D  STAAAS                 1      4  0
010000961010     D  STALNP                 5      7  0
010100961010     D  STANRS                 8      9  0
010200961010     D  STANSP                10     16  0
010300961010     D  STASPE                 1     16
010400961010     D  STASTS                17     19
010500961010     D  STAERR                20     22
010600961010     D*  DS x rottura livello spedizione - codice errore
010700961010     D SAVSPE          DS
010800040305     D  CNI_WSV                3     16  0
010900961010     D  WSVAAS                 1      4  0
011000961010     D  WSVLNP                 5      7  0
011100961010     D  WSVNRS                 8      9  0
011200961010     D  WSVNSP                10     16  0
011300961015     D  WSVSPE                 1     16
011400961010     D  WSVSTS                17     19
011500961010     D  WSVERR                20     22
011600960528     D WLBDA8          DS
011700960528     D  G02DAT                 1      8  0
011800960528     D  G02INV                 9     16  0
011900960528     D  G02ERR                17     17
012000960528     D  G02TGI                18     22  0
012100110630      *----------------------------------------------------*
012200110630      *  Invio E-mail
012300110630     D Indirizzi       s            253
012400110630     D Oggetto         s             44
012500110630     D Messaggio       s           5000
012600110630      *
012700110630     d   DSmail        ds
012800110630     D Mail_Ind                      44
012900110630     d   IND_char                     1    dim(44)
013000110630      *
013100110630     D Lunghezza_Indirizzo...
013200110630     D                 s              5u 0
013300110630     C*---------------------------------------------------------------*
013400110630     D MsgDSP          S            256                                         Testo generico
013500110630     C*---------------------------------------------------------------*
013600110630     D SndMg01         S             10                                         Message type
013700110630     D                                     INZ('*INFO')
013800110630     D SndMg02         S             10                                         Deluvery mode
013900110630     D                                     INZ('*BREAK')
014000110630     D SndMg03         S            256                                         Message text
014100110630     D SndMg04         S             10I 0                                      Length of text
014200110630     D                                     INZ(%SIZE(SndMg03))
014300110630     D SndMg05         S             10                                         User profile
014400110630     D                                     DIM(299)
014500110630     D SndMg06         S             10I 0                                      Number of user
014600110630     D SndMg07         S             10I 0                                      Message sent indic.
014700110630     D SndMg08         S             10I 0                                      Function requested
014800110630     D SndMg10         S              1                                         Show display
014900110630     D                                     INZ('N')
015000110630     D SndMg11         S             20                                         Qualified MSGQ name
015100110630     D SndMg12         S              4                                         Name type indicator
015200110630     D                                     INZ('*USR')
015300110630      * indice di scihera
015400110630     D Jx              S              5I 0
015500110630      *
015600110630     D/COPY QSYSINC/QRPGLESRC,QUSEC
015700110630      *
015800120905     d bart_it         C                   CONST('@Brt.it;')
015900120905     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
016000110630
016100960708     D*
016200980421     D COST1           C                   CONST('IT/BAR/IT1   ')
016300110330     D CALB_Plat       C                   CONST('039IT04507990150')
016400960528     I*---------------------------------------------------------------------*
016500960528     I* FILE
016600960528     I*---------------------------------------------------------------------*
016700960528     IIFTSTA00
016800960528     I              SSIFTSTA                    STADAT
016900960528     C*---------------------------------------------------------------------*
017000960528     C* Ciclo principale
017100960528     C*---------------------------------------------------------------------*
017200960528      * Posizionamento iniziale sul file  (prima lettura)
017300961010     C                   EXSR      POSFIL
017400961010      *
017500961010      * Loop letture fino a fine file EDSTA
017600961010     C     *IN30         DOWEQ     '0'
017700961010      * Se ho già scritto 999 spedizioni devo scrivere un nuovo msg
017800961010     C* Caricamento NUMERATORE
017900961010     C                   EXSR      CARNUM
018000961010      * Eseguo scrittura dati record ST00
018100961010     C                   EXSR      WRST00
018200961010      * Eseguo scrittura dati record ST05
018300961010     C                   EXSR      WRST05
018400961010      * Eseguo scrittura dati record ST10
018500961010     C                   EXSR      WRST10
018600961010      * Eseguo scrittura dati record ST15
018700961010     C                   EXSR      WRST15
018800961010      *
018900961010      * Se cambia il nr.CMR o se ho già scritto 999 bolle scrivo
019000961010      * intestazione nuovo messaggio
019100961010     C     STACMR        DOWEQ     WSVCMR
019200961010     C     WPROG         ANDLT     999
019300961010     C     *IN30         ANDEQ     '0'
019400961010      * Salvataggio dati spedizione - errore
019500961010     C                   EXSR      SAVDAT
019600961010      *
019700961010      * Loop lettura bolla x nr.spedizione codice errore
019800961010     C     SPEERR        DOWEQ     SAVSPE
019900961010     C     STACMR        ANDEQ     WSVCMR
020000961010     C     *IN30         ANDEQ     '0'
020100961010     C                   EXSR      GESBOL
020200961010      * Lettura file
020300961010     C                   EXSR      REDFIL
020400961010     C                   END
020500961010      *
020600960528      * Eseguo scrittura dati record SD00
020700961015     C     WSVSTS        IFEQ      *BLANKS
020800961015     C     WSVERR        ANDEQ     *BLANKS
020900961015     C     §PUDTB        ANDEQ     'E'
021000961015     C     WSVSTS        OREQ      *BLANKS
021100961015     C     WSVERR        ANDEQ     *BLANKS
021200961015     C     §PUSGD        ANDEQ     'S'
021300961015     C     WSVSTS        ORNE      *BLANKS
021400961015     C     WSVERR        ORNE      *BLANKS
021500960528     C                   EXSR      WRSD00
021600960528      * Eseguo scrittura dati record SD05
021700110330     C                   MOVEL     'N'           WRTEVE            1
021800960528     C                   EXSR      WRSD05
021900110330      * Scrivo tipi record successivi solo se ho scritto SD00 e SD05
022000110330     C     WRTEVE        IFEQ      'S'
022100960528      * Eseguo scrittura dati record SD10
022200960528     C                   EXSR      WRSD10
022300960528      * Eseguo scrittura dati record SD15
022400960528     C                   EXSR      WRSD15
022500960528      * Eseguo scrittura dati record SD20
022600960528     C                   EXSR      WRSD20
022700110330     C                   ENDif
022800961015     C                   END
022900961015     C*
023000960528     C                   END
023100961010     C                   END
023200110418      * Chiusura messaggio
023300110418     C                   EXSR      end_MSG
023400110418      *
023500010409      *  PRIMA DI USCIRE IMPOSA IL TIPO DI TRASMISSIONE (DA TAB."PT")
023600010409      *   VIA INTESA O VIA FTP DA SEDE UTILIZZANDO L'ULTIMO BYTE
023700010409      *     A DESTRA DELLA KPJBU POICHÈ NELLA KPJBU DA SINISTRA CI
023800010409      *      SONO LE ALTRE INFORMAZIONI RELATIVE ALLA DS TRTC78DS.
023900010410     C                   MOVEL     *BLANK        MEMBR0           10
024000010410     C                   MOVEL     'M'           MEMBR8            8
024100010410     C                   MOVE      §PTKSC        MEMBR8
024200010410     C                   MOVEL     MEMBR8        MEMBR0
024300010410     C                   MOVEL     MEMBR0        FLD11            11
024400010410     C                   MOVE      §PTFTT        FLD11            11
024500010410     C                   MOVE      FLD11         KPJBU
024600960528     C* Fine pgm.
024700121010      *   se si tratta di un traduttore NON EDIFACT deve far saltare al CL la parte
024800121010      *    successiva di trasmissione
024900121010     c                   if        §PZPGMTRSS = 'GELSTA'
025000131007     c                                or
025100131007     c                             §PZPGMTRSS = 'AMZSTA'
025200121010     c                   eval      Righe_Dett = 'N'
025300121010     c                   end
025400121010      *
025500960528     C                   SETON                                        LR
025600961010     C*----------------------------------------------------------------
025700961010     C*? POSFIL - POSIZIONAMENTO PRIMO RECORD
025800961010     C*----------------------------------------------------------------
025900961010     C     POSFIL        BEGSR
026000961010     C*
026100961010     C* Mi posiziono su EDSTA con cod.cliente - nr.CMR.
026200980401     C     D87LNP        IFEQ      0
026300980401     C                   Z-ADD     D87CCM        KKEY1
026400980401     C                   ELSE
026500980401     C                   Z-ADD     D87LNP        KKEY1
026600980401     C                   END
026700961010     C                   Z-ADD     D87NFE        KNFE
026800961010     C     KSTA          CHAIN     EDSTAWRK                           30
026900110330      *   Se fine File
027000110330     C                   If        *IN30 = *on
027100110330     C                   MOVEL     'S'           WFINE             1
027200110330     C                   EndIF
027300110330      *
027400961010     C                   ENDSR
027500960528     C*----------------------------------------------------------------
027600960528     C*? REDFIL - LETTURA PROSSIMO RECORD
027700960528     C*----------------------------------------------------------------
027800960528     C     REDFIL        BEGSR
027900960528     C*
028000961010     C* Deleto vecchio record
028100961010     C                   DELETE    EDSTA000
028200961010     C* Lettura file EDSTA
028300961010     C     KSTA          READE     EDSTAWRK                               30
028400110330      *   Se fine File
028500110330     C                   If        *IN30 = *on
028600110330     C                   MOVEL     'S'           WFINE             1
028700110330     C                   EndIF
028800960528     C*
028900960528     C                   ENDSR
029000961010     C*----------------------------------------------------------------
029100110414     C*? chiusura Messaggio ULTIMI Segmenti da riportare
029200110414     C*----------------------------------------------------------------
029300110414     C     End_MSG       BEGSR
029400110414      *
029500110414      *   e deve essere tradotto NON CON INTESA....
029600110414     c                   If        §PZPGMTRSS = 'STA94A'
029700110414     C                               and  Numero_Righe > 1
029800120831     c                               or
029900120831     c                             §PZPGMTRSS = 'GELSTA'
030000120831     C                               and  Numero_Righe > 0
030100131007     c                               or
030200131007     c                             §PZPGMTRSS = 'AMZSTA'
030300131007     C                               and  Numero_Righe > 0
030400110414      *
030500110414      * Se c'è stato qualche evento da inviare che quindi ha scritto
030600110414      *   chiude il messaggio altrimenti esegue un ROLLBACK
030700110414     c                   if        Righe_Dett = 'S'
030800120831      *
030900110414      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
031000110414      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
031100110414     c                   move      §MSNUM        fine04            4
031200110414     c                   eval      STATPR = 'FINE'
031300110414     c                   eval      STADAT = fine04
031400120831      *
031500120831     c                   exsr      write_OUTPUT
031600110414     c                   eval      Righe_Dett = 'N'
031700110414     c                   else
031800120831      *
031900120831      * deve uscire senza avere fatto nulla
032000110414     c                   eval      STATPR = 'RLBK'
032100120831     c                   exsr      write_OUTPUT
032200110414      *
032300110414     c                   end
032400110414     c                   end
032500110414     C*
032600110414     C                   ENDSR
032700110414     C*----------------------------------------------------------------
032800961010     C*? SAVDAT - SALVATAGGIO DATI PER SCRITTURA BOLLA
032900961010     C*----------------------------------------------------------------
033000961010     C     SAVDAT        BEGSR
033100961010     C*
033200961010     C*  Mi imposto i dati spedizione dalla prima bolla letta
033300961010     C*  per poi eseguire scrittura dettaglio a rottura bolla
033400961010     C*  codice errore
033500961010     C                   MOVEL     SPEERR        SAVSPE
033600040305     C                   MOVEL     CNI_STA       CNI_WSV
033700961010     C                   MOVEL     STACNI        WSVCNI
033800980429     C                   MOVEL     STADTR        WSVDTR
033900980429     C                   MOVEL     STAHMS        WSVHMS
034000961010     C                   MOVEA     *BLANKS       SGN
034100961010     C                   MOVEL     *BLANKS       WDAT
034200961010     C                   Z-ADD     0             WNRCOL            5 0
034300961010     C                   Z-ADD     0             Y                 3 0
034400961010     C*
034500961010     C                   ENDSR
034600961010     C*----------------------------------------------------------------
034700961010     C*? GESBOL - Carico dati sgn
034800961010     C*----------------------------------------------------------------
034900961010     C     GESBOL        BEGSR
035000961010     C*
035100961010     C                   ADD       1             WNRCOL
035200961010     C*  Eseguo caricamento det.colli se devo inviarli
035300961010     C     §PUSGD        IFEQ      'S'                                          sgn sempre
035400961010     C     §PUSGD        OREQ      'N'                                          sgn negat.
035500980402     C     STAFLE        ANDNE     *BLANKS                                      errore
035600980330     C* Controllo se devo tornare segnacollo euroexpress
035700980330     C     §PUBS1        IFEQ      'E'
035800980330     C                   EXSR      CARSGE
035900980330     C                   ELSE
036000961010     C* Calcolo nr.segnacollo da passare al cliente
036100961015     C                   EXSR      CARSGN
036200961010     C                   ADD       1             Y
036300961010     C                   MOVEL     WSGN          SGN(Y)
036400961010     C                   END
036500980330     C                   END
036600961010     C*
036700961010     C                   ENDSR
036800960528     C*----------------------------------------------------------------
036900960528     C*? WRST00 - Scrittura record partner
037000960528     C*----------------------------------------------------------------
037100960528     C     WRST00        BEGSR
037200960528     C*
037300960528     C*  Pulisco dati DS
037400960528     C                   CLEAR                   EDST00
037500960528     C*  Mittente: Bartolini  (ITBAR + AS..)
037600960528     C                   MOVEL     'ITBAR'       WMITT             8
037700961002     C                   MOVE      WAS400        WMITT             8
037800960530     C                   MOVEL     WMITT         TA0004
037900960729     C                   MOVEL     'ZZ'          TA0007
038000961010     C                   MOVEL     D87CPT        TA0010
038100961029     C                   MOVE      '    '        TA0010
038200960827     C*  Utilizzo qualificatore x destinatario msg.
038300960827     C                   MOVEL     §PTMSQ        TAA007
038400960528     C*  Data/Ora invio messagio: adesso
038500980109     C                   MOVEL     WOGGI         TA0017
038600960528     C                   MOVEL     WHHMM         TA0019
038700960528     C*  Codice documento - nome documento
038800961002     C                   MOVEL     '781'         TA1001
038900961002     C                   MOVEL     D87NFE        TA1004                         Nr.F.entrata
039000960528     C                   MOVEL     '9  '         TA1225
039100961010     C*  Data foglio entrata
039200961010     C                   MOVEL     '137'         TA2005
039300961010     C                   MOVEL     D87DFE        TA2380                         Data F.E.
039400960528     C                   MOVEL     '102'         TA2379
039500100512      *
039600100512      *    Se si vuole la data con l'orario
039700100512     c                   if        §pudate203 ='S'
039800100512     C                   MOVE      Whhmm         TA2380
039900100512     C                   MOVEL(p)  '203'         TA2379
040000100512     c                   end
040100100512      *
040200960528     C*  Scivo record IFTSTA
040300960705     C                   CLEAR                   WDAT
040400960705     C                   MOVEL     EDST00        WDAT
040500960726     C                   Z-ADD     §MSNUM        STAPRG
040600960726     C                   MOVEL     'ST00'        STATPR
040700960705     C                   MOVEL     WDAT          STADAT
040800110330      *
040900120831      *  Testata messaggio  UNB-BGM-DTM
041000120831     c                   exsr      write_OUTPUT
041100110330      *
041200960528     C                   ENDSR
041300960708     C*----------------------------------------------------------------
041400960708     C*? WRST05 - Scrittura record partner
041500960708     C*----------------------------------------------------------------
041600960708     C     WRST05        BEGSR
041700960708     C*
041800960708     C                   CLEAR                   EDST05
041900960708     C*  Ship from:
042000960712     C                   MOVEL     *BLANKS       WDAT
042100960708     C                   MOVEL     'SF '         TE3035
042200980421     C                   MOVEL     §PTPLT        TE3036
042300960708     C*  Scivo record IFCSUM
042400960708     C                   CLEAR                   WDAT
042500960708     C                   MOVEL     EDST05        WDAT
042600960726     C                   Z-ADD     §MSNUM        STAPRG
042700960712     C                   MOVEL     'ST05'        STATPR
042800960708     C                   MOVEL     WDAT          STADAT
042900110330      *
043000120831      *  Testata messaggio  NAD-SF
043100120831     c                   exsr      write_OUTPUT
043200960708     C*  Ship to:
043300960712     C                   MOVEL     *BLANKS       WDAT
043400960708     C                   CLEAR                   EDST05
043500960708     C                   MOVEL     'ST '         TE3035
043600980421     C                   MOVEL     COST1         TE3036
043700110330      *
043800110330      *  x CALBERSON --> GEODIS
043900110330      *      imposta la Platform che ci hanno chiesto di utilizzare
044000110330      *      x identificare diversamente dai dati di DUNLOP altrimenti
044100110330      *      non riescono a dividerli nel loro sistema.
044200110330      *  da impostare il §PTLOC ??????
044300110330     c                   if        §PTKSC = 493298
044400110330     C                   clear                   TE3036
044500110330     C                   MOVEL     CALB_Plat     TE3036
044600110330     c                   end
044700110330      *
044800960708     C*  Scivo record IFCSUM
044900960708     C                   CLEAR                   WDAT
045000960708     C                   MOVEL     EDST05        WDAT
045100960726     C                   Z-ADD     §MSNUM        STAPRG
045200960712     C                   MOVEL     'ST05'        STATPR
045300960708     C                   MOVEL     WDAT          STADAT
045400110330      *
045500110330      *  Testata messaggio  NAD-ST
045600120831     c                   exsr      write_OUTPUT
045700960708     C*
045800960708     C                   ENDSR
045900960528     C*----------------------------------------------------------------
046000960528     C*? WRST10 - Scrittura record partner
046100960528     C*----------------------------------------------------------------
046200960528     C     WRST10        BEGSR
046300960528     C*
046400960528     C*  Pulisco dati DS
046500110330     C                   MOVEL     STACMR        WSVCMR
046600980402     C     §PTCM1        IFEQ      'S'
046700980402     C     §PTNF1        OREQ      'S'
046800960712     C                   MOVEL     *BLANKS       WDAT
046900960528     C                   CLEAR                   EDST10
047000960726     C*  Se devo ritornare imposto AFB + nr.messaggio
047100961010     C                   MOVEL     STATPD        TB1153
047200961010     C                   MOVEL     STACMR        TB1154
047300961010     C                   MOVEL     STACMR        WSVCMR
047400960726     C*  Scivo record IFCSUM
047500960726     C                   CLEAR                   WDAT
047600960726     C                   MOVEL     EDST10        WDAT
047700960726     C                   Z-ADD     §MSNUM        STAPRG
047800960726     C                   MOVEL     'ST10'        STATPR
047900960726     C                   MOVEL     WDAT          STADAT
048000110330      *
048100120831      *  Testata messaggio  RFF-LOC
048200120831     c                   exsr      write_OUTPUT
048300110330      *
048400980402     C                   END
048500960528     C*
048600960528     C                   ENDSR
048700960528     C*----------------------------------------------------------------
048800960528     C*? WRST15 - Scrittura record partner
048900960528     C*----------------------------------------------------------------
049000960528     C     WRST15        BEGSR
049100960528     C*
049200960528     C*  Pulisco dati DS
049300960712     C                   MOVEL     *BLANKS       WDAT
049400960528     C                   CLEAR                   EDST15
049500960528     C*
049600960528     C                   ENDSR
049700960528     C*----------------------------------------------------------------
049800960528     C*? WRSD00 - Scrittura record partner - dettaglio
049900960528     C*----------------------------------------------------------------
050000960528     C     WRSD00        BEGSR
050100960528     C*
050200960528     C*  Pulisco dati DS
050300960712     C                   MOVEL     *BLANKS       WDAT
050400960528     C                   CLEAR                   EDSD00
050500140417      *
050600140417     c* informazioni sulla Bolla
050700140417     c                   eval      trovato_BLP = 'N'
050800140417     c     kBLP01wsv     chain     fnblp01l
050900140417     C                   if        %Found(fnblp01l)
051000140417     c                   eval      trovato_BLP = 'S'
051100140417     c                   end
051200140417      *
051300120705      * Default
051400120705      *  se non richiesta l'inversione del dato CNI nell'AGE
051500120705     c                   if        §ptTRD <> 'S'
051600120705      *
051700960528     C*  Imposto numero spedizione cliente
051800040305     C                   MOVEL     CNI_WSV       DA1004
051900120705      *
052000120705      *  inverte con l'AGE
052100120705     c                   elseIF    §ptTRD  = 'S'
052200120705     C                   MOVEL(p)  WSVCNI        DA1004
052300120705     c                   end
052400120705      *
052500961010      * se non è dischetto lo segnalo
052600961015     C   01
052700961015     CANN01WSVCMR        IFEQ      *BLANKS
052800960821     C                   MOVEL     'S'           WSD15             1
052900961015     C                   ENDIF
053000961010     C*  Incremento proggr. a rottura nr.spedizione
053100961015     C     WSVSPE        IFNE      WEXSPE
053200961015     C                   MOVEL     WSVSPE        WEXSPE           16
053300961010     C                   END
053400060406      ** Deve sempre incrementare
053500060406     C                   ADD       1             WPROG
053600961010     C                   MOVEL     WPROG         DA1490
053700961010     C                   MOVEL     '9999'        DA1312
053800961010     C*  Scivo record IFTSTA
053900961010     C                   CLEAR                   WDAT
054000961010     C                   MOVEL     EDSD00        WDAT
054100961010     C                   Z-ADD     §MSNUM        STAPRG
054200961010     C                   MOVEL     'SD00'        STATPR
054300961010     C                   MOVEL     WDAT          STADAT
054400110330      *
054500120831      *  Testata messaggio  CNI-
054600120831     c                   exsr      write_OUTPUT
054700960528     C*
054800960528     C                   ENDSR
054900960528     C*----------------------------------------------------------------
055000960528     C*? WRSD05 - Scrittura record partner - dettaglio
055100960528     C*----------------------------------------------------------------
055200960528     C     WRSD05        BEGSR
055300960528     C*
055400960528     C*  Pulisco dati DS
055500960712     C                   MOVEL     *BLANKS       WDAT
055600960528     C                   CLEAR                   EDSD05
055700961010     C                   MOVEL     'AGE'         DB1153
055800120705      * Default
055900120705      *  se non richiesta l'inversione del dato CNI nell'AGE
056000120705     c                   if        §ptTRD <> 'S'
056100961010     C                   MOVEL     WSVCNI        DB1154
056200120705      *  inverte con il CNI
056300120705     c                   elseIF    §ptTRD  = 'S'
056400120705     C                   MOVEL     CNI_WSV       DB1154
056500120705     c                   end
056600120705      *
056700961015     C                   MOVEL     WSVSTS        DB9011
056800961015     C                   MOVEL     WSVERR        DB9013
056900960528     C                   MOVEL     '7  '         DB2005
057000961010     C*  Controllo se ho anche l'ora o solo la data
057100980429     C     WSVHMS        IFEQ      *ZEROS
057200980429     C                   MOVEL     WSVDTR        DB2380
057300960528     C                   MOVEL     '102'         DB2379
057400100512      *
057500100512      *    Se si vuole la data con l'orario
057600100512     c                   if        §pudate203 ='S'
057700100512     C                   MOVE      '0001'        DB2380
057800100512     C                   MOVEL(p)  '203'         DB2379
057900100512     c                   end
058000100512      *
058100961010     C                   ELSE
058200980429     C                   MOVEL     WSVDTR        WDATHH           12
058300980429     C                   MOVE      WSVHMS        WDATHH
058400961010     C                   MOVEL     WDATHH        DB2380
058500961010     C                   MOVEL     '203'         DB2379
058600961010     C                   END
058700140416      *-------
058800140416      *  Se richiesto l'invio del RIF.NUMERICO RFF+CU
058900140416      *    pilotato dalla tab.PT EDISDPS --> §PSRCU ='S' deve
059000140416      *    impostare i campi aggiunti sulla EDSD05DS (DB1153A/54A) (A) mentre
059100140416      *    i (B) sono stati aggiunti ma non ancora gestiti (x futuro)
059200140416     C                   if        Invio_RFF_CU = 'S'
059300140416      *--
059400140417      *  da BLP ha i campi della BOLLA
059500140417     C                   if        trovato_BLP = 'S' and BLPrmn > 0
059600140416     C                   eval      DB1153A = 'CU'
059700140416     C                   eval      DB1154A = %Trim(%Editc(blpRMN:'Z'))
059800140416     c                   end
059900140416      *--
060000140416     c                   end
060100140416      *-------
060200960917     C*  Scivo record IFTSTA
060300960917     C                   CLEAR                   WDAT
060400961010     C                   MOVEL     EDSD05        WDAT
060500960917     C                   Z-ADD     §MSNUM        STAPRG
060600961010     C                   MOVEL     'SD05'        STATPR
060700960917     C                   MOVEL     WDAT          STADAT
060800110330      *
060900120831      *  Testata messaggio  STS-RFF-DTM
061000120831     c                   exsr      write_OUTPUT
061100110330      *
061200110330     C                   MOVEL     'S'           WRTEVE            1
061300110330     c                   eval      Righe_Dett = 'S'
061400110330      *
061500960528     C     FIND05        ENDSR
061600960528     C*-------------------------------------------------------------*
061700960528     C*? WRSD10 - Scrittura record partner - dettaglio
061800960528     C*----------------------------------------------------------------
061900960528     C     WRSD10        BEGSR
062000960528     C*
062100960528     C*  Pulisco dati DS
062200961010     C                   MOVEL     *BLANKS       WDAT
062300960528     C                   CLEAR                   EDSD10
062400960528     C*
062500960528     C                   ENDSR
062600960528     C*-------------------------------------------------------------*
062700960528     C*? WRSD15 - Scrittura record partner - dettaglio
062800960528     C*----------------------------------------------------------------
062900960528     C     WRSD15        BEGSR
063000960528     C*
063100960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
063200960821     C     WSD15         IFEQ      'S'
063300960821     C                   MOVEL     *BLANKS       WDAT
063400960821     C                   CLEAR                   EDSD15
063500960821     C                   MOVEL     'AAI'         DD4451
063600960821     C                   MOVEL     FTX(1)        DD4440
063700960821     C                   MOVEL     EDSD15        WDAT
063800960821     C                   Z-ADD     §MSNUM        STAPRG
063900960821     C                   MOVEL     'SD15'        STATPR
064000960821     C                   MOVEL     WDAT          STADAT
064100110330      *
064200120831      *  Testata messaggio  FTX-
064300120831     c                   exsr      write_OUTPUT
064400110330      *
064500110330     c                   eval      Righe_Dett = 'S'
064600960821     C                   CLEAR                   WSD15
064700960821     C                   END
064800961010     C*
064900960528     C                   ENDSR
065000960528     C*-------------------------------------------------------------*
065100960528     C*? WRSD20 - Scrittura record partner - dettaglio
065200960528     C*----------------------------------------------------------------
065300960528     C     WRSD20        BEGSR
065400960528     C*
065500960528     C*  Pulisco dati DS
065600960528     C                   CLEAR                   EDSD20
065700960528     C                   MOVEL     '99999'       DE1496
065800961010     C*  carico dettaglio segnacolli
065900960912     C                   EXSR      CARCOL
066000960528     C*
066100960528     C                   ENDSR
066200960912     C*----------------------------------------------------------------
066300960912     C*? CARCOL - Routine caricamento colli da dettag.
066400960912     C*----------------------------------------------------------------
066500960912     C     CARCOL        BEGSR
066600960912     C*
066700961010     C*  Scrivo D20 da SGN
066800961029     C     Y             IFNE      0
066900961010     C                   MOVE      Y             DE7224
067000961015     C                   MOVEL     'PK'          DE7065
067100960912     C                   Z-ADD     0             WW                3 0
067200040218     C                   Z-ADD     0             WE                3 0
067300040217      *  imposta quante volte deve eseguire il ciclo per quanti segnacolli
067400040217      *  imposta per ogni record.
067500050428     c                   if        §Puel2 = 0
067600050428     c                   eval      §Puel2 = 1
067700050428     c                   end
067800050428      *
067900040217     c     y             div       §Puel2        volte
068000040217     c                   mvr                     resto             3 0
068100040217     c                   if        resto > 0
068200040217     c                   add       1             volte
068300040217     c                   end
068400040217      *
068500040217     C                   DO        volte         vl                3 0
068600040218      * Impacchetta sulla schiera di 20 elementi quanti elementi servono
068700040218     C                   DO        §PUEL2        WW
068800040218     c                   add       1             WE
068900040218     C                   MOVEL     SGN(WE)       D20(WW)
069000040218     c                   ENDDO
069100040218      *
069200980424     C     D20(1)        IFNE      *BLANKS
069300960913     C                   MOVEL     '24'          DE4233
069400980424     C                   ELSE
069500980424     C                   MOVEL     *BLANKS       DE4233
069600980424     C                   MOVEA     *BLANKS       D20
069700980424     C                   END
069800040218      *
069900960912     C                   EXSR      WRT20
070000040218      *
070100960912     C*  Se ci sono ancora dei colli clear tracciato
070200960912     C*  e scrittura nuovo record
070300960912     C                   MOVEL     *BLANKS       EDSD20
070400960912     C                   END
070500961029     C*
070600961029     C                   ELSE
070700961029     C                   CLEAR                   EDSD20
070800961104     C                   MOVEL     '99999'       DE1496
070900961029     C                   MOVE      WNRCOL        DE7224
071000961029     C                   MOVEL     'PK'          DE7065
071100961029     C                   EXSR      WRT20
071200961029     C                   END
071300960912     C*
071400960912     C                   ENDSR
071500960912     C*----------------------------------------------------------------
071600960912     C*? WRT20 - scrivo record d20
071700960912     C*----------------------------------------------------------------
071800960912     C     WRT20         BEGSR
071900960912     C*
072000960912     C*  Scrivo record SD20
072100051202     C*    ma solo se contiene delle informazioni altrimenti salto
072200960912     C                   MOVEL     EDSD20        WDAT
072300051202     C*
072400051202     c                   if        %subst(WDAT:10:1900) <> *blanks
072500960912     C                   MOVEL     'SD20'        STATPR
072600960912     C                   Z-ADD     §MSNUM        STAPRG
072700960912     C                   MOVEL     WDAT          STADAT
072800110330      *
072900120831      *  Testata messaggio  GID-PCI
073000120831     c                   exsr      write_OUTPUT
073100110330     c                   eval      Righe_Dett = 'S'
073200051202     c                   end
073300960912     C*
073400960912     C                   ENDSR
073500960730     C*----------------------------------------------------------------
073600960912     C*? CARSGN - caricamneto segnacollo
073700960730     C*----------------------------------------------------------------
073800960912     C     CARSGN        BEGSR
073900960730     C*
074000960730     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
074100960912     C                   MOVEA     *BLANKS       C35
074200960912     C                   MOVEL     *BLANKS       WSGN             35
074300960912     C*  Compongo lnp se la devo passare
074400960912     C     §PULP2        IFNE      0
074500960912     C                   Z-ADD     §PULP2        WA                3 0
074600961029     C                   MOVEL     STAFLS        WLNP              3
074700960912     C                   MOVEA     WLNP          C35(WA)
074800960912     C                   END
074900960912     C*  Compongo lna se la devo passare
075000960912     C     §PULA2        IFNE      0
075100960912     C                   Z-ADD     §PULA2        WA                3 0
075200961010     C                   MOVEL     STALNA        WLNA              3
075300960912     C                   MOVEA     WLNA          C35(WA)
075400960912     C                   END
075500960912     C*  Compongo nrs se lo devo passare
075600960912     C     §PUNR2        IFNE      0
075700960912     C                   Z-ADD     §PUNR2        WA                3 0
075800961010     C                   MOVEL     STANRS        WNRS              2
075900960912     C                   MOVEA     WNRS          C35(WA)
076000960912     C                   END
076100960912     C*  Compongo nsc se la devo passare
076200960912     C     §PUSG2        IFNE      0
076300960912     C                   Z-ADD     §PUSG2        WA                3 0
076400961010     C                   MOVEL     STANSC        WNSC              7
076500960912     C                   MOVEA     WNSC          C35(WA)
076600960912     C                   END
076700960912     C*  Compongo znc se la devo passare
076800960912     C     §PUZN2        IFNE      0
076900960912     C                   Z-ADD     §PUZN2        WA                3 0
077000970626     C                   MOVE      STAZNC        WZNC              2
077100960912     C                   MOVEA     WZNC          C35(WA)
077200960912     C                   END
077300960912     C*  metto C35 in WSGN
077400960912     C                   MOVEA     C35           WSGN
077500960730     C*
077600960730     C                   ENDSR
077700980330     C*----------------------------------------------------------------
077800980330     C*? CARSGE - caricamneto segnacollo Euroexpress
077900980330     C*----------------------------------------------------------------
078000980330     C     CARSGE        BEGSR
078100980330     C*
078200980401     C* Controllo se ho aggiunto eccedenze euroexpress
078300980401     C     STANSC        IFEQ      *ZEROS
078400980401     C                   MOVEL     *BLANKS       WSVCNI
078500980402     C                   MOVEL     '0'           WSVCNI
078600980401     C                   Z-ADD     1             Y
078700980401     C                   MOVEL     STACNI        SGN(Y)
078800980401     C* Abblenco SAVSPE
078900980401     C                   MOVEL     *BLANKS       SAVSPE
079000980402     C                   MOVEL     *LOVAL        WEXSPE
079100980401     C                   ELSE
079200980401     C* Aggancio segnacollo eroexpress
079300980401     C                   Z-ADD     STAFLS        KFLS
079400980401     C                   Z-ADD     STALNA        KLNA
079500980401     C                   Z-ADD     STANRS        KNRS
079600980401     C                   Z-ADD     STANSC        KNSC
079700980330     C                   MOVEL     'E'           KTRC
079800051017     C                   CLEAR                   FIARS000
079900051017     C     KARS          CHAIN     FIARS01L                           33
080000061031      *
080100061031      *  Se si tratta di un cliente che riceve i dati non IFCSUM ma da VAB
080200061031      *  i tipi records su ARS sono classificati come "C" diskC
080300061031     C   33              MOVEL     'C'           KTRC
080400061031     C   33KARS          CHAIN     FIARS01L                           33
080500980330     C                   ADD       1             Y
080600051017     C  N33              MOVEL     ARSNOT        SGN(Y)
080700980401     C                   END
080800980330     C*
080900980330     C                   ENDSR
081000960927     C*----------------------------------------------------------------
081100960927     C*? CARNUM - CARICAMENTO NUMERATORE
081200960927     C*----------------------------------------------------------------
081300960927     C     CARNUM        BEGSR
081400960927     C*
081500960927     C                   Z-ADD     0             WPROG             4 0
081600960927     C                   Z-ADD     0             X                 3 0
081700960927     C                   Z-ADD     1             §MSNUM
081800961010     C                   MOVEL     'MS'          KCOD
081900961010     C                   MOVEL     *BLANKS       KKEY
082000961010     C                   MOVEL     '781'         KKEY
082100961010     C     KTAB          CHAIN     EDTAB01L                           30
082200960927     C     *IN30         IFEQ      '0'
082300960927     C     TABFLG        ANDEQ     *BLANKS
082400960927     C                   MOVEL     TABUNI        DSMS
082500960927     C                   ADD       1             §MSNUM
082600061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
082700061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
082800061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
082900061219     c                   move      §MSNUM        campo_4n          4 0
083000061219     c                   if        campo_4n = 0
083100061219     c                   add       1             §MSNUM
083200061219     c                   end
083300960927     C                   Z-ADD     WOGGI         §MSDAT
083400960927     C                   MOVEL     DSMS          TABUNI
083500960927     C                   UPDATE    EDTAB000
083600960927     C                   END
083700960927     C*
083800960927     C                   ENDSR
083900960528     C*----------------------------------------------------------------
084000960528     C*? *INZSR - OPERAZIONI INIZIALI
084100960528     C*----------------------------------------------------------------
084200960528     C     *INZSR        BEGSR
084300960530     C*
084400960530     C     *ENTRY        PLIST
084500960530     C                   PARM                    KPJBA
084600110630     C                   PARM                    Righe_Dett
084700110630     C                   PARM                    Esito             1            ritorno errore
084800110630      *
084900110630      * reset campo compattazione x Download file
085000110630     C                   eval      esito = '0'
085100110630     c                   eval      Compatta_80 = 'S'
085200110704     c                   eval      Righe_Dett = 'N'
085300110601      *
085400961010     C                   MOVEL     KPJBU         TRTC87
085500110601      *
085600960528     C* Definisco chiavi di accesso
085700961010     C     KTAB          KLIST
085800961010     C                   KFLD                    KCOD
085900961010     C                   KFLD                    KKEY
086000120831      *
086100961010     C     KSTA          KLIST
086200980401     C                   KFLD                    KKEY1
086300961010     C                   KFLD                    KNFE
086400120831      *
086500051017     C     KARS          KLIST
086600980330     C                   KFLD                    KFLS
086700980330     C                   KFLD                    KLNA
086800980330     C                   KFLD                    KNRS
086900980330     C                   KFLD                    KNSC
087000980330     C                   KFLD                    KTRC
087100120831      *
087200120831      * DEVE ESSERE PILOTATO DALLA TAB.3K
087300120831     C     KTAB2         KLIST
087400120831     C                   KFLD                    TBLKUT
087500120831     C                   KFLD                    TBLCOD
087600120831     C                   KFLD                    TBLKEY
087700120831      *
087800140417     C     kBLP01wsv     KLIST
087900140417     C                   KFLD                    wsvAAS
088000140417     C                   KFLD                    wsvLNP
088100140417     C                   KFLD                    wsvNRS
088200140417     C                   KFLD                    wsvNSP
088300140416      *
088400960528     C* Definizione campi di work
088500051017     C     *LIKE         DEFINE    ARSFLS        KFLS
088600051017     C     *LIKE         DEFINE    arSLNA        KLNA
088700051017     C     *LIKE         DEFINE    arSNRS        KNRS
088800051017     C     *LIKE         DEFINE    arSNSC        KNSC
088900051017     C     *LIKE         DEFINE    arSTRC        KTRC
089000980330     C     *LIKE         DEFINE    TABCOD        KCOD
089100961010     C     *LIKE         DEFINE    TABKEY        KKEY
089200980401     C     *LIKE         DEFINE    STAKEY        KKEY1
089300961010     C     *LIKE         DEFINE    STANFE        KNFE
089400961010     C     *LIKE         DEFINE    STACMR        WSVCMR
089500961010     C     *LIKE         DEFINE    STACNI        WSVCNI
089600980429     C     *LIKE         DEFINE    STADTR        WSVDTR
089700980429     C     *LIKE         DEFINE    STAHMS        WSVHMS
089800040217     C     *LIKE         DEFINE    y             volte
089900960528     C*
090000960528     C* Recupero codice AS
090100960528     C                   Z-ADD     1             CODUT             1 0
090200960705     C                   CALL      'X§PARUT'
090300960528     C                   PARM                    UTEDSE
090400960528     C                   MOVEL     REC80         CNCR80
090500980622     C                   MOVEL     '005'         WAS400            3
090600960528     C* Recupero data e ora x messaggio
090700960528     C                   TIME                    WHHDAT           14 0
090800960708     C                   MOVE      WHHDAT        G02DAT
090900960528     C                   MOVE      WHHDAT        DATA              6 0
091000960528     C                   MOVEL     *BLANK        G02ERR
091100960528     C                   CALL      'XSRDA8'
091200960528     C                   PARM                    WLBDA8
091300960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
091400960708     C                   MOVEL     WHHDAT        WHHMM             4 0
091500960528     C                   MOVE      WOGGI         WOGGI6            6 0
091600960528     C                   MOVE      WHHDAT        WANNO             2 0
091700960528     C                   MOVEL     WANNO         WOGGI6
091800960528     C* Caricamento Tabella Partner esteri
091900960528     C                   Z-ADD     0             X                 3 0
092000961015     C                   MOVEL     'PT'          KCOD
092100961015     C                   MOVEL     D87CPT        KKEY
092200120831     c                   clear                   dsPT
092300961010     C     KTAB          CHAIN     EDTAB01L                           30
092400961010     C     *IN30         IFEQ      '0'
092500961010     C     TABFLG        ANDEQ     *BLANKS
092600961010     C                   MOVEL     TABUNI        DSPT
092700960528     C                   END
092800961015     C                   MOVEL     'PU'          KCOD
092900120831     c                   clear                   dsPU
093000961010     C     KTAB          CHAIN     EDTAB01L                           30
093100961010     C     *IN30         IFEQ      '0'
093200961010     C     TABFLG        ANDEQ     ' '
093300961010     C                   MOVEL     TABUNI        DSPU
093400960912     C                   END
093500110330      *   Con quale traduttore deve essere scaricato e scritto il File ????
093600110330     C                   MOVEL     'PZ'          KCOD
093700120831     c                   clear                   dsPZ
093800110330     C     KTAB          CHAIN     EDTAB01L
093900110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
094000110330     C                   MOVEL     TABUNI        DSPZ
094100110330     C                   END
094200120831      *
094300140416      *----
094400140416      *   Si vuole inviare anche il riferimento NUMERICO ricevuto oltre l'AGE ?
094500140416     C                   MOVEL     'PS'          KCOD
094600140416     c                   clear                   dsPS
094700140416     C                   eval      Invio_RFF_CU = *Blank
094800140416     C     KTAB          CHAIN     EDTAB01L
094900140416     C                   if        %Found(EDTAB01L) and tabflg = *blank
095000140416     C                   MOVEL     TABUNI        DSPS
095100140416     C                   eval      Invio_RFF_CU = §psRCU
095200140416     C                   END
095300140416      *----
095400140416      *
095500120831     c                   eval      CLIENTE =  '0' + %editc(§PTKSC:'X')
095600120831     C                   Z-ADD     1             TBLKUT
095700120831     C                   MOVEL     '3K'          TBLCOD
095800120831     C                   moveL(P)  §PTKSC        TBLKEY
095900120831     C     KTAB2         chain     tabel00f
096000120831     c                   if        %Found(tabel00f)
096100120831     c                   eval      ds3K = tblUNI
096200120831     c                   eval      CLIENTE =  '0' + %editc(§3Kcks:'X')
096300120831     C                   END
096400960528     C* finta read x IFTSTA
096500960528     C   01
096600110407     CANN01              READ      EDSTA00F                               01
096700960528     C*
096800960528     C                   ENDSR
096900120831      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
097000120831      *  Scrive l'OUTPUT da inviare
097100120831      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
097200120831     C     write_OUTPUT  BEGSR
097300120831      *
097400120831      * Scrittura file per 20/210 Appuntamento da ATR
097500120831     c                   if        STATPR <> 'FINE' and
097600120831     c                             STATPR <> 'RLBK'
097700120831     C                   WRITE     IFTSTA00
097800120831     c                   end
097900120831      *
098000120831     c                   If        §PZPGMTRSS = 'STA94A'
098100120831     c                   exsr      Exit_segmenti
098200120831     c                   elseIf    §PZPGMTRSS = 'GELSTA'
098300120831     c                   exsr      Exit_FileGEL
098400131007     c                   elseIf    §PZPGMTRSS = 'AMZSTA'
098500131007     c                   exsr      Exit_FileAMZ
098600120831     C                   EnD
098700120831      *
098800120831     C                   ENDSR
098900110330      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
099000110330      *  Scrittura dei segmenti IFTSTA D94A
099100110330      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
099200110330     C     Exit_Segmenti BEGSR
099300120413      *
099400110330     c                   eval      Tipo_DS = STATPR
099500110330     c                   eval      Suffisso   = §PUSIG
099600110601     c                   eval      Compatta_80 = §pucompat
099700110330     c                   clear                   DsGenerica
099800110330     c                   eval      DsGenerica = STADAT
099900110330     c                   call      'TRTCT81R1'
100000110330     c                   PARM                    CLIENTE           8
100100110330     c                   PARM                    Suffisso          2
100200110330     c                   PARM                    Tipo_DS           4
100300110330     c                   PARM                    DsGenerica     1024
100400110330     C                   PARM                    Numero_Righe      5 0
100500110601     c                   PARM                    Compatta_80       1
100600110617     C                   PARM                    Progressivo      10
100700110621     c                   PARM                    esito             1
100800110630     C
100900110630     C                   if        esito = 'E'
101000110630     c                   exsr      Invio_Mail
101100110630     c                   end
101200110330      *
101300110330     C                   ENDSR
101400120831     c*==================================================================*
101500120831      * Manda un File a tracciato NETEXPRESS EUROPE
101600120831     c*==================================================================*
101700120831     c     Exit_FileGEL  begsr
101800120831      *
101900120831     c                   clear                   DsGenerica
102000120831     c                   eval      Tipo_DS = STATPR
102100120831     c                   eval      DsGenerica = STADAT
102200120905     c                   call      'TRTCT81GR'
102300120831     c                   PARM                    CLIENTE
102400120831     c                   PARM                    Tipo_DS           4
102500120831     c                   PARM                    DsGenerica     1024
102600120831     C                   PARM                    Numero_Righe      5 0
102700120831     C                   PARM                    Progressivo      10
102800120831     c                   PARM                    esito             1
102900120831     C
103000120831     C                   if        esito = 'E'
103100120831     c                   exsr      Invio_Mail
103200120831     c                   end
103300120831      *
103400120831     C                   ENDSR
103500131007     c*==================================================================*
103600131007      * Manda un File a tracciato IFTSTA Particolarissimo di AMAZON
103700131007     c*==================================================================*
103800131007     c     Exit_FileAMZ  begsr
103900131007      *
104000131007     c                   clear                   DsGenerica
104100131007     c                   eval      Tipo_DS = STATPR
104200131007     c                   eval      DsGenerica = STADAT
104300131007     c                   call      'TRTCT81AR'
104400131007     c                   PARM                    CLIENTE
104500131007     c                   PARM                    Tipo_DS           4
104600131007     c                   PARM                    DsGenerica     1024
104700131007     C                   PARM                    Numero_Righe      5 0
104800131007     C                   PARM                    Progressivo      10
104900131007     c                   PARM                    esito             1
105000131007     C
105100131007     C                   if        esito = 'E'
105200131007     c                   exsr      Invio_Mail
105300131007     c                   end
105400131007      *
105500131007     C                   ENDSR
105600110630     c*==================================================================*
105700110630      * Manda un Msg x E-mail
105800110630     c*==================================================================*
105900110630     c     Invio_Mail    begsr
106000110630      *
106100120831     c                   clear                   CLIENTEksc        8
106200120831     c                   eval      CLIENTEksc =  '0' + %editc(§PTKSC:'X')
106300130821???? C                   EVAL      Oggetto ='TRTCT88R DOWNLD ATR STATI EDI !!'
106400110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione ATR EDI-
106500110630     C                              STATI NON tradotto x invio su DOWNLD. -
106600110630     C                             Controllare il Download del Cliente. << '+
106700120831     c                             CLIENTEksc + ' >>'
106800110630      *
106900110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
107000110630     C* Inizializzo variabili
107100110630     C                   movel     *blanks       wrkEml          253
107200110630     C                   movel     *blanks       wrkMsg         5000
107300110630     C                   movel     *blanks       wrkOgg           44
107400110630      *
107500110630     C* Valorizzo i campi della e-m@ail - indirizzo
107600110630     C                   eval      wrkEml = CED_BART
107700110630     C                   eval      wrkOgg = Oggetto
107800110630     C                   eval      wrkMsg = Messaggio
107900110630     C                   call(e)   'TRTCT00R2'
108000110630     C                   parm                    wrkEml
108100110630     C                   parm                    wrkOgg
108200110630     C                   parm                    wrkMsg
108300110630     C*
108400110630     C                   ENDSR
108500110630      * ?------------------------------------------------------------------ */
108600110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
108700110630      * ?------------------------------------------------------------------ */
108800110630     C     *pssr         BEGSR
108900110630     C
109000110630     C                   eval      esito = 'E'
109100110630     c                   exsr      Invio_Mail
109200110630     C
109300110630     C                   ENDSR     '*CANCL'
109400110630     C
109500110630      *  ------------------------------------------------------------------ */
109600961010** FTX
109700961010RECEIVED REFERRING TO A SHIPMENT NOT RECEIVED WITH E.D.I. MESSAGE
