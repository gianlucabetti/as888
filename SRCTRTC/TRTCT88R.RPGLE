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
040100140911      *
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
057000140911      *
057100961010     C*  Controllo se ho anche l'ora o solo la data
057200980429     C     WSVHMS        IFEQ      *ZEROS
057300140911      *
057400980429     C                   MOVEL     WSVDTR        DB2380
057500960528     C                   MOVEL     '102'         DB2379
057600100512      *
057700100512      *    Se si vuole la data con l'orario
057800100512     c                   if        §pudate203 ='S'
057900100512     C                   MOVE      '0001'        DB2380
058000100512     C                   MOVEL(p)  '203'         DB2379
058100100512     c                   end
058200100512      *
058300961010     C                   ELSE
058400140911      *
058500980429     C                   MOVEL     WSVDTR        WDATHH           12
058600980429     C                   MOVE      WSVHMS        WDATHH
058700961010     C                   MOVEL     WDATHH        DB2380
058800961010     C                   MOVEL     '203'         DB2379
058900140911      *
059000961010     C                   END
059100140911      *
059200140911      * Se presente l'orario, controllo sempre la data/ora xché NON sia
059300140911      *  mai superiore alla Data di elaborazione del messaggio
059400140911      *   x non inviare logicamente una data successiva all'elaborazione stessa.
059500140911     C                   If          DB2379 = '203'
059600140911      *
059700140911     C                   MOVE      DB2380        Data12_0         12 0
059800140911     c                   if        Data12_0 > DataOraOggi
059900140911     c                   movel     DataOraOggi   DB2380
060000140911     c                   end
060100140911      *
060200140911     c                   end
060300140416      *-------
060400140416      *  Se richiesto l'invio del RIF.NUMERICO RFF+CU
060500140416      *    pilotato dalla tab.PT EDISDPS --> §PSRCU ='S' deve
060600140416      *    impostare i campi aggiunti sulla EDSD05DS (DB1153A/54A) (A) mentre
060700140416      *    i (B) sono stati aggiunti ma non ancora gestiti (x futuro)
060800140416     C                   if        Invio_RFF_CU = 'S'
060900140416      *--
061000140417      *  da BLP ha i campi della BOLLA
061100140417     C                   if        trovato_BLP = 'S' and BLPrmn > 0
061200140416     C                   eval      DB1153A = 'CU'
061300140416     C                   eval      DB1154A = %Trim(%Editc(blpRMN:'Z'))
061400140416     c                   end
061500140416      *--
061600140416     c                   end
061700140416      *-------
061800960917     C*  Scivo record IFTSTA
061900960917     C                   CLEAR                   WDAT
062000961010     C                   MOVEL     EDSD05        WDAT
062100960917     C                   Z-ADD     §MSNUM        STAPRG
062200961010     C                   MOVEL     'SD05'        STATPR
062300960917     C                   MOVEL     WDAT          STADAT
062400110330      *
062500120831      *  Testata messaggio  STS-RFF-DTM
062600120831     c                   exsr      write_OUTPUT
062700110330      *
062800110330     C                   MOVEL     'S'           WRTEVE            1
062900110330     c                   eval      Righe_Dett = 'S'
063000110330      *
063100960528     C     FIND05        ENDSR
063200960528     C*-------------------------------------------------------------*
063300960528     C*? WRSD10 - Scrittura record partner - dettaglio
063400960528     C*----------------------------------------------------------------
063500960528     C     WRSD10        BEGSR
063600960528     C*
063700960528     C*  Pulisco dati DS
063800961010     C                   MOVEL     *BLANKS       WDAT
063900960528     C                   CLEAR                   EDSD10
064000960528     C*
064100960528     C                   ENDSR
064200960528     C*-------------------------------------------------------------*
064300960528     C*? WRSD15 - Scrittura record partner - dettaglio
064400960528     C*----------------------------------------------------------------
064500960528     C     WRSD15        BEGSR
064600960528     C*
064700960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
064800960821     C     WSD15         IFEQ      'S'
064900960821     C                   MOVEL     *BLANKS       WDAT
065000960821     C                   CLEAR                   EDSD15
065100960821     C                   MOVEL     'AAI'         DD4451
065200960821     C                   MOVEL     FTX(1)        DD4440
065300960821     C                   MOVEL     EDSD15        WDAT
065400960821     C                   Z-ADD     §MSNUM        STAPRG
065500960821     C                   MOVEL     'SD15'        STATPR
065600960821     C                   MOVEL     WDAT          STADAT
065700110330      *
065800120831      *  Testata messaggio  FTX-
065900120831     c                   exsr      write_OUTPUT
066000110330      *
066100110330     c                   eval      Righe_Dett = 'S'
066200960821     C                   CLEAR                   WSD15
066300960821     C                   END
066400961010     C*
066500960528     C                   ENDSR
066600960528     C*-------------------------------------------------------------*
066700960528     C*? WRSD20 - Scrittura record partner - dettaglio
066800960528     C*----------------------------------------------------------------
066900960528     C     WRSD20        BEGSR
067000960528     C*
067100960528     C*  Pulisco dati DS
067200960528     C                   CLEAR                   EDSD20
067300960528     C                   MOVEL     '99999'       DE1496
067400961010     C*  carico dettaglio segnacolli
067500960912     C                   EXSR      CARCOL
067600960528     C*
067700960528     C                   ENDSR
067800960912     C*----------------------------------------------------------------
067900960912     C*? CARCOL - Routine caricamento colli da dettag.
068000960912     C*----------------------------------------------------------------
068100960912     C     CARCOL        BEGSR
068200960912     C*
068300961010     C*  Scrivo D20 da SGN
068400961029     C     Y             IFNE      0
068500961010     C                   MOVE      Y             DE7224
068600961015     C                   MOVEL     'PK'          DE7065
068700960912     C                   Z-ADD     0             WW                3 0
068800040218     C                   Z-ADD     0             WE                3 0
068900040217      *  imposta quante volte deve eseguire il ciclo per quanti segnacolli
069000040217      *  imposta per ogni record.
069100050428     c                   if        §Puel2 = 0
069200050428     c                   eval      §Puel2 = 1
069300050428     c                   end
069400050428      *
069500040217     c     y             div       §Puel2        volte
069600040217     c                   mvr                     resto             3 0
069700040217     c                   if        resto > 0
069800040217     c                   add       1             volte
069900040217     c                   end
070000040217      *
070100040217     C                   DO        volte         vl                3 0
070200040218      * Impacchetta sulla schiera di 20 elementi quanti elementi servono
070300040218     C                   DO        §PUEL2        WW
070400040218     c                   add       1             WE
070500040218     C                   MOVEL     SGN(WE)       D20(WW)
070600040218     c                   ENDDO
070700040218      *
070800980424     C     D20(1)        IFNE      *BLANKS
070900960913     C                   MOVEL     '24'          DE4233
071000980424     C                   ELSE
071100980424     C                   MOVEL     *BLANKS       DE4233
071200980424     C                   MOVEA     *BLANKS       D20
071300980424     C                   END
071400040218      *
071500960912     C                   EXSR      WRT20
071600040218      *
071700960912     C*  Se ci sono ancora dei colli clear tracciato
071800960912     C*  e scrittura nuovo record
071900960912     C                   MOVEL     *BLANKS       EDSD20
072000960912     C                   END
072100961029     C*
072200961029     C                   ELSE
072300961029     C                   CLEAR                   EDSD20
072400961104     C                   MOVEL     '99999'       DE1496
072500961029     C                   MOVE      WNRCOL        DE7224
072600961029     C                   MOVEL     'PK'          DE7065
072700961029     C                   EXSR      WRT20
072800961029     C                   END
072900960912     C*
073000960912     C                   ENDSR
073100960912     C*----------------------------------------------------------------
073200960912     C*? WRT20 - scrivo record d20
073300960912     C*----------------------------------------------------------------
073400960912     C     WRT20         BEGSR
073500960912     C*
073600960912     C*  Scrivo record SD20
073700051202     C*    ma solo se contiene delle informazioni altrimenti salto
073800960912     C                   MOVEL     EDSD20        WDAT
073900051202     C*
074000051202     c                   if        %subst(WDAT:10:1900) <> *blanks
074100960912     C                   MOVEL     'SD20'        STATPR
074200960912     C                   Z-ADD     §MSNUM        STAPRG
074300960912     C                   MOVEL     WDAT          STADAT
074400110330      *
074500120831      *  Testata messaggio  GID-PCI
074600120831     c                   exsr      write_OUTPUT
074700110330     c                   eval      Righe_Dett = 'S'
074800051202     c                   end
074900960912     C*
075000960912     C                   ENDSR
075100960730     C*----------------------------------------------------------------
075200960912     C*? CARSGN - caricamneto segnacollo
075300960730     C*----------------------------------------------------------------
075400960912     C     CARSGN        BEGSR
075500960730     C*
075600960730     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
075700960912     C                   MOVEA     *BLANKS       C35
075800960912     C                   MOVEL     *BLANKS       WSGN             35
075900960912     C*  Compongo lnp se la devo passare
076000960912     C     §PULP2        IFNE      0
076100960912     C                   Z-ADD     §PULP2        WA                3 0
076200961029     C                   MOVEL     STAFLS        WLNP              3
076300960912     C                   MOVEA     WLNP          C35(WA)
076400960912     C                   END
076500960912     C*  Compongo lna se la devo passare
076600960912     C     §PULA2        IFNE      0
076700960912     C                   Z-ADD     §PULA2        WA                3 0
076800961010     C                   MOVEL     STALNA        WLNA              3
076900960912     C                   MOVEA     WLNA          C35(WA)
077000960912     C                   END
077100960912     C*  Compongo nrs se lo devo passare
077200960912     C     §PUNR2        IFNE      0
077300960912     C                   Z-ADD     §PUNR2        WA                3 0
077400961010     C                   MOVEL     STANRS        WNRS              2
077500960912     C                   MOVEA     WNRS          C35(WA)
077600960912     C                   END
077700960912     C*  Compongo nsc se la devo passare
077800960912     C     §PUSG2        IFNE      0
077900960912     C                   Z-ADD     §PUSG2        WA                3 0
078000961010     C                   MOVEL     STANSC        WNSC              7
078100960912     C                   MOVEA     WNSC          C35(WA)
078200960912     C                   END
078300960912     C*  Compongo znc se la devo passare
078400960912     C     §PUZN2        IFNE      0
078500960912     C                   Z-ADD     §PUZN2        WA                3 0
078600970626     C                   MOVE      STAZNC        WZNC              2
078700960912     C                   MOVEA     WZNC          C35(WA)
078800960912     C                   END
078900960912     C*  metto C35 in WSGN
079000960912     C                   MOVEA     C35           WSGN
079100960730     C*
079200960730     C                   ENDSR
079300980330     C*----------------------------------------------------------------
079400980330     C*? CARSGE - caricamneto segnacollo Euroexpress
079500980330     C*----------------------------------------------------------------
079600980330     C     CARSGE        BEGSR
079700980330     C*
079800980401     C* Controllo se ho aggiunto eccedenze euroexpress
079900980401     C     STANSC        IFEQ      *ZEROS
080000980401     C                   MOVEL     *BLANKS       WSVCNI
080100980402     C                   MOVEL     '0'           WSVCNI
080200980401     C                   Z-ADD     1             Y
080300980401     C                   MOVEL     STACNI        SGN(Y)
080400980401     C* Abblenco SAVSPE
080500980401     C                   MOVEL     *BLANKS       SAVSPE
080600980402     C                   MOVEL     *LOVAL        WEXSPE
080700980401     C                   ELSE
080800980401     C* Aggancio segnacollo eroexpress
080900980401     C                   Z-ADD     STAFLS        KFLS
081000980401     C                   Z-ADD     STALNA        KLNA
081100980401     C                   Z-ADD     STANRS        KNRS
081200980401     C                   Z-ADD     STANSC        KNSC
081300980330     C                   MOVEL     'E'           KTRC
081400051017     C                   CLEAR                   FIARS000
081500051017     C     KARS          CHAIN     FIARS01L                           33
081600061031      *
081700061031      *  Se si tratta di un cliente che riceve i dati non IFCSUM ma da VAB
081800061031      *  i tipi records su ARS sono classificati come "C" diskC
081900061031     C   33              MOVEL     'C'           KTRC
082000061031     C   33KARS          CHAIN     FIARS01L                           33
082100980330     C                   ADD       1             Y
082200051017     C  N33              MOVEL     ARSNOT        SGN(Y)
082300980401     C                   END
082400980330     C*
082500980330     C                   ENDSR
082600960927     C*----------------------------------------------------------------
082700960927     C*? CARNUM - CARICAMENTO NUMERATORE
082800960927     C*----------------------------------------------------------------
082900960927     C     CARNUM        BEGSR
083000960927     C*
083100960927     C                   Z-ADD     0             WPROG             4 0
083200960927     C                   Z-ADD     0             X                 3 0
083300960927     C                   Z-ADD     1             §MSNUM
083400961010     C                   MOVEL     'MS'          KCOD
083500961010     C                   MOVEL     *BLANKS       KKEY
083600961010     C                   MOVEL     '781'         KKEY
083700961010     C     KTAB          CHAIN     EDTAB01L                           30
083800960927     C     *IN30         IFEQ      '0'
083900960927     C     TABFLG        ANDEQ     *BLANKS
084000960927     C                   MOVEL     TABUNI        DSMS
084100960927     C                   ADD       1             §MSNUM
084200061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
084300061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
084400061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
084500061219     c                   move      §MSNUM        campo_4n          4 0
084600061219     c                   if        campo_4n = 0
084700061219     c                   add       1             §MSNUM
084800061219     c                   end
084900960927     C                   Z-ADD     WOGGI         §MSDAT
085000960927     C                   MOVEL     DSMS          TABUNI
085100960927     C                   UPDATE    EDTAB000
085200960927     C                   END
085300960927     C*
085400960927     C                   ENDSR
085500960528     C*----------------------------------------------------------------
085600960528     C*? *INZSR - OPERAZIONI INIZIALI
085700960528     C*----------------------------------------------------------------
085800960528     C     *INZSR        BEGSR
085900960530     C*
086000960530     C     *ENTRY        PLIST
086100960530     C                   PARM                    KPJBA
086200110630     C                   PARM                    Righe_Dett
086300110630     C                   PARM                    Esito             1            ritorno errore
086400110630      *
086500110630      * reset campo compattazione x Download file
086600110630     C                   eval      esito = '0'
086700110630     c                   eval      Compatta_80 = 'S'
086800110704     c                   eval      Righe_Dett = 'N'
086900110601      *
087000961010     C                   MOVEL     KPJBU         TRTC87
087100110601      *
087200960528     C* Definisco chiavi di accesso
087300961010     C     KTAB          KLIST
087400961010     C                   KFLD                    KCOD
087500961010     C                   KFLD                    KKEY
087600120831      *
087700961010     C     KSTA          KLIST
087800980401     C                   KFLD                    KKEY1
087900961010     C                   KFLD                    KNFE
088000120831      *
088100051017     C     KARS          KLIST
088200980330     C                   KFLD                    KFLS
088300980330     C                   KFLD                    KLNA
088400980330     C                   KFLD                    KNRS
088500980330     C                   KFLD                    KNSC
088600980330     C                   KFLD                    KTRC
088700120831      *
088800120831      * DEVE ESSERE PILOTATO DALLA TAB.3K
088900120831     C     KTAB2         KLIST
089000120831     C                   KFLD                    TBLKUT
089100120831     C                   KFLD                    TBLCOD
089200120831     C                   KFLD                    TBLKEY
089300120831      *
089400140417     C     kBLP01wsv     KLIST
089500140417     C                   KFLD                    wsvAAS
089600140417     C                   KFLD                    wsvLNP
089700140417     C                   KFLD                    wsvNRS
089800140417     C                   KFLD                    wsvNSP
089900140416      *
090000960528     C* Definizione campi di work
090100051017     C     *LIKE         DEFINE    ARSFLS        KFLS
090200051017     C     *LIKE         DEFINE    arSLNA        KLNA
090300051017     C     *LIKE         DEFINE    arSNRS        KNRS
090400051017     C     *LIKE         DEFINE    arSNSC        KNSC
090500051017     C     *LIKE         DEFINE    arSTRC        KTRC
090600980330     C     *LIKE         DEFINE    TABCOD        KCOD
090700961010     C     *LIKE         DEFINE    TABKEY        KKEY
090800980401     C     *LIKE         DEFINE    STAKEY        KKEY1
090900961010     C     *LIKE         DEFINE    STANFE        KNFE
091000961010     C     *LIKE         DEFINE    STACMR        WSVCMR
091100961010     C     *LIKE         DEFINE    STACNI        WSVCNI
091200980429     C     *LIKE         DEFINE    STADTR        WSVDTR
091300980429     C     *LIKE         DEFINE    STAHMS        WSVHMS
091400040217     C     *LIKE         DEFINE    y             volte
091500960528     C*
091600960528     C* Recupero codice AS
091700960528     C                   Z-ADD     1             CODUT             1 0
091800960705     C                   CALL      'X§PARUT'
091900960528     C                   PARM                    UTEDSE
092000960528     C                   MOVEL     REC80         CNCR80
092100980622     C                   MOVEL     '005'         WAS400            3
092200960528     C* Recupero data e ora x messaggio
092300960528     C                   TIME                    WHHDAT           14 0
092400960708     C                   MOVE      WHHDAT        G02DAT
092500960528     C                   MOVE      WHHDAT        DATA              6 0
092600960528     C                   MOVEL     *BLANK        G02ERR
092700960528     C                   CALL      'XSRDA8'
092800960528     C                   PARM                    WLBDA8
092900960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
093000960708     C                   MOVEL     WHHDAT        WHHMM             4 0
093100140911      *
093200140911      * data e orario del TIME da confrontare in seguito
093300140911     c                   clear                   DataOraOggi      12 0
093400140911     c                   eval        DataOraOggi = Woggi  * 10000 + wHHmm
093500140911      *
093600140911      * si memorizza sempre la data del Foglio con l'orario del Time
093700140911      *  del messaggio (perchè non esiste altrove) da confrontare in seguito
093800140911     c                   clear                   DataOraFoglio    12 0
093900140911     c                   eval      DataOraFoglio = D87dfe * 10000 + wHHmm
094000140911      *
094100960528     C                   MOVE      WOGGI         WOGGI6            6 0
094200960528     C                   MOVE      WHHDAT        WANNO             2 0
094300960528     C                   MOVEL     WANNO         WOGGI6
094400960528     C* Caricamento Tabella Partner esteri
094500960528     C                   Z-ADD     0             X                 3 0
094600961015     C                   MOVEL     'PT'          KCOD
094700961015     C                   MOVEL     D87CPT        KKEY
094800120831     c                   clear                   dsPT
094900961010     C     KTAB          CHAIN     EDTAB01L                           30
095000961010     C     *IN30         IFEQ      '0'
095100961010     C     TABFLG        ANDEQ     *BLANKS
095200961010     C                   MOVEL     TABUNI        DSPT
095300960528     C                   END
095400961015     C                   MOVEL     'PU'          KCOD
095500120831     c                   clear                   dsPU
095600961010     C     KTAB          CHAIN     EDTAB01L                           30
095700961010     C     *IN30         IFEQ      '0'
095800961010     C     TABFLG        ANDEQ     ' '
095900961010     C                   MOVEL     TABUNI        DSPU
096000960912     C                   END
096100110330      *   Con quale traduttore deve essere scaricato e scritto il File ????
096200110330     C                   MOVEL     'PZ'          KCOD
096300120831     c                   clear                   dsPZ
096400110330     C     KTAB          CHAIN     EDTAB01L
096500110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
096600110330     C                   MOVEL     TABUNI        DSPZ
096700110330     C                   END
096800120831      *
096900140416      *----
097000140416      *   Si vuole inviare anche il riferimento NUMERICO ricevuto oltre l'AGE ?
097100140416     C                   MOVEL     'PS'          KCOD
097200140416     c                   clear                   dsPS
097300140416     C                   eval      Invio_RFF_CU = *Blank
097400140416     C     KTAB          CHAIN     EDTAB01L
097500140416     C                   if        %Found(EDTAB01L) and tabflg = *blank
097600140416     C                   MOVEL     TABUNI        DSPS
097700140416     C                   eval      Invio_RFF_CU = §psRCU
097800140416     C                   END
097900140416      *----
098000140416      *
098100120831     c                   eval      CLIENTE =  '0' + %editc(§PTKSC:'X')
098200120831     C                   Z-ADD     1             TBLKUT
098300120831     C                   MOVEL     '3K'          TBLCOD
098400120831     C                   moveL(P)  §PTKSC        TBLKEY
098500120831     C     KTAB2         chain     tabel00f
098600120831     c                   if        %Found(tabel00f)
098700120831     c                   eval      ds3K = tblUNI
098800120831     c                   eval      CLIENTE =  '0' + %editc(§3Kcks:'X')
098900120831     C                   END
099000960528     C* finta read x IFTSTA
099100960528     C   01
099200110407     CANN01              READ      EDSTA00F                               01
099300960528     C*
099400960528     C                   ENDSR
099500120831      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
099600120831      *  Scrive l'OUTPUT da inviare
099700120831      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
099800120831     C     write_OUTPUT  BEGSR
099900120831      *
100000120831      * Scrittura file per 20/210 Appuntamento da ATR
100100120831     c                   if        STATPR <> 'FINE' and
100200120831     c                             STATPR <> 'RLBK'
100300120831     C                   WRITE     IFTSTA00
100400120831     c                   end
100500120831      *
100600120831     c                   If        §PZPGMTRSS = 'STA94A'
100700120831     c                   exsr      Exit_segmenti
100800120831     c                   elseIf    §PZPGMTRSS = 'GELSTA'
100900120831     c                   exsr      Exit_FileGEL
101000131007     c                   elseIf    §PZPGMTRSS = 'AMZSTA'
101100131007     c                   exsr      Exit_FileAMZ
101200120831     C                   EnD
101300120831      *
101400120831     C                   ENDSR
101500110330      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
101600110330      *  Scrittura dei segmenti IFTSTA D94A
101700110330      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
101800110330     C     Exit_Segmenti BEGSR
101900120413      *
102000110330     c                   eval      Tipo_DS = STATPR
102100110330     c                   eval      Suffisso   = §PUSIG
102200110601     c                   eval      Compatta_80 = §pucompat
102300110330     c                   clear                   DsGenerica
102400110330     c                   eval      DsGenerica = STADAT
102500110330     c                   call      'TRTCT81R1'
102600110330     c                   PARM                    CLIENTE           8
102700110330     c                   PARM                    Suffisso          2
102800110330     c                   PARM                    Tipo_DS           4
102900110330     c                   PARM                    DsGenerica     1024
103000110330     C                   PARM                    Numero_Righe      5 0
103100110601     c                   PARM                    Compatta_80       1
103200110617     C                   PARM                    Progressivo      10
103300110621     c                   PARM                    esito             1
103400110630     C
103500110630     C                   if        esito = 'E'
103600110630     c                   exsr      Invio_Mail
103700110630     c                   end
103800110330      *
103900110330     C                   ENDSR
104000120831     c*==================================================================*
104100120831      * Manda un File a tracciato NETEXPRESS EUROPE
104200120831     c*==================================================================*
104300120831     c     Exit_FileGEL  begsr
104400120831      *
104500120831     c                   clear                   DsGenerica
104600120831     c                   eval      Tipo_DS = STATPR
104700120831     c                   eval      DsGenerica = STADAT
104800120905     c                   call      'TRTCT81GR'
104900120831     c                   PARM                    CLIENTE
105000120831     c                   PARM                    Tipo_DS           4
105100120831     c                   PARM                    DsGenerica     1024
105200120831     C                   PARM                    Numero_Righe      5 0
105300120831     C                   PARM                    Progressivo      10
105400120831     c                   PARM                    esito             1
105500120831     C
105600120831     C                   if        esito = 'E'
105700120831     c                   exsr      Invio_Mail
105800120831     c                   end
105900120831      *
106000120831     C                   ENDSR
106100131007     c*==================================================================*
106200131007      * Manda un File a tracciato IFTSTA Particolarissimo di AMAZON
106300131007     c*==================================================================*
106400131007     c     Exit_FileAMZ  begsr
106500131007      *
106600131007     c                   clear                   DsGenerica
106700131007     c                   eval      Tipo_DS = STATPR
106800131007     c                   eval      DsGenerica = STADAT
106900131007     c                   call      'TRTCT81AR'
107000131007     c                   PARM                    CLIENTE
107100131007     c                   PARM                    Tipo_DS           4
107200131007     c                   PARM                    DsGenerica     1024
107300131007     C                   PARM                    Numero_Righe      5 0
107400131007     C                   PARM                    Progressivo      10
107500131007     c                   PARM                    esito             1
107600131007     C
107700131007     C                   if        esito = 'E'
107800131007     c                   exsr      Invio_Mail
107900131007     c                   end
108000131007      *
108100131007     C                   ENDSR
108200110630     c*==================================================================*
108300110630      * Manda un Msg x E-mail
108400110630     c*==================================================================*
108500110630     c     Invio_Mail    begsr
108600110630      *
108700120831     c                   clear                   CLIENTEksc        8
108800120831     c                   eval      CLIENTEksc =  '0' + %editc(§PTKSC:'X')
108900130821???? C                   EVAL      Oggetto ='TRTCT88R DOWNLD ATR STATI EDI !!'
109000110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione ATR EDI-
109100110630     C                              STATI NON tradotto x invio su DOWNLD. -
109200110630     C                             Controllare il Download del Cliente. << '+
109300120831     c                             CLIENTEksc + ' >>'
109400110630      *
109500110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
109600110630     C* Inizializzo variabili
109700110630     C                   movel     *blanks       wrkEml          253
109800110630     C                   movel     *blanks       wrkMsg         5000
109900110630     C                   movel     *blanks       wrkOgg           44
110000110630      *
110100110630     C* Valorizzo i campi della e-m@ail - indirizzo
110200110630     C                   eval      wrkEml = CED_BART
110300110630     C                   eval      wrkOgg = Oggetto
110400110630     C                   eval      wrkMsg = Messaggio
110500110630     C                   call(e)   'TRTCT00R2'
110600110630     C                   parm                    wrkEml
110700110630     C                   parm                    wrkOgg
110800110630     C                   parm                    wrkMsg
110900110630     C*
111000110630     C                   ENDSR
111100110630      * ?------------------------------------------------------------------ */
111200110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
111300110630      * ?------------------------------------------------------------------ */
111400110630     C     *pssr         BEGSR
111500110630     C
111600110630     C                   eval      esito = 'E'
111700110630     c                   exsr      Invio_Mail
111800110630     C
111900110630     C                   ENDSR     '*CANCL'
112000110630     C
112100110630      *  ------------------------------------------------------------------ */
112200961010** FTX
112300961010RECEIVED REFERRING TO A SHIPMENT NOT RECEIVED WITH E.D.I. MESSAGE
