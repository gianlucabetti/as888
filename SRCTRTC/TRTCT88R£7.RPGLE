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
002100960528     D*---------------------------------------------------------------------*
002200960528     D* Schiere
002300960528     D*---------------------------------------------------------------------*
002400960528     D* Schiera per scrittura dati località record ST10
002500960528     D* Schiera per scrittura testi liberi record ST15
002600960528     D* Schiera per scrittura testi liberi record SD15
002700960528     D* Schiera per reperimento nr.seganacollo
002800961010     D SGN             S             35    DIM(500)                             Sgn.Sped..
002900960528     D* Tabella codici eventi
003000960528     D CEV             S             35    DIM(200)
003100960528     D C2A             S              3    DIM(200)
003200960612     D STS             S              3    DIM(200)
003300960821     D* TESTO LIBERO
003400960821     D FTX             S             70    DIM(1) CTDATA PERRCD(1)
003500960912     D* composizione schiera 35 chr x segnacollo
003600960912     D C35             S              1    DIM(35)
003700960528     D*---------------------------------------------------------------------*
003800960528     D* DS
003900960528     D*---------------------------------------------------------------------*
004000960528     D* .. per scompozione dati da spedire a seconda del tipo record
004100960528     D EDST00        E DS                  EXTNAME(EDST00DS)
004200960708     D EDST05        E DS                  EXTNAME(EDST05DS)
004300960528     D EDST10        E DS                  EXTNAME(EDST10DS)
004400960528     D  T10                   48    103
004500960528     D                                     DIM(2)
004600960528     D EDST15        E DS                  EXTNAME(EDST15DS)
004700960528     D  T15                   13    362
004800960528     D                                     DIM(5)
004900960528     D EDSD00        E DS                  EXTNAME(EDSD00DS)
005000960528     D EDSD05        E DS                  EXTNAME(EDSD05DS)
005100960528     D EDSD10        E DS                  EXTNAME(EDSD10DS)
005200960528     D EDSD15        E DS                  EXTNAME(EDSD15DS)
005300960528     D  D15                   13    362
005400960528     D                                     DIM(5)
005500960528     D EDSD20        E DS                  EXTNAME(EDSD20DS)
005600960730     D  D20                   33    732
005700960730     D                                     DIM(20)
005800051202      **
005900960528     D*  Ds per scrittura dati località
006000960528     D WST10           DS
006100960528     D  WTPL                   1      3
006200960528     D  WLOC                   4     28
006300960528     D*---------------------------------------------------------------------*
006400110407     D* .. per reperimento tipo record di EDSTA00F da scrivere
006500960528     D*                    .. 5 - 8  : tipo record
006600040218     D WDAT            DS          1950
006700960729     D  STAPRG                 1      4  0
006800960726     D  STATPR                 5      8
006900960528     D*---------------------------------------------------------------------*
007000960528     D KPJBA         E DS
007100960528     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
007200960528     D CNCR80        E DS
007300120413     D DS3K          E DS
007400961016     D DSPT          E DS                  EXTNAME(EDIDSPT)
007500960912     D DSPU          E DS                  EXTNAME(EDIDSPU)
007600110330     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
007700110330     D DSPW          E DS                  EXTNAME(EDIDSPW)
007800110330     D DSPV          E DS                  EXTNAME(EDIDSPV)
007900960726     D DSMS          E DS                  EXTNAME(EDIDSMS)
008000961010     D TRTC87        E DS                  EXTNAME(TRTC87DS)
008100110330     d Righe_Dett      s              1
008200960528     D*  DS x scomposizione segnacollo
008300960528     D DSSGN           DS
008400960528     D  SGNLNP                 1      3  0
008500960528     D  SGNLNA                 4      6  0
008600960528     D  SGNNRS                 7      8  0
008700960528     D  SGNNSC                 9     15  0
008800960829     D  SGNZNC                16     17  0
008900961010     D*  DS x rottura livello spedizione - codice errore
009000961010     D SPEERR          DS
009100040305     D  CNI_STA                3     16  0
009200961010     D  STAAAS                 1      4  0
009300961010     D  STALNP                 5      7  0
009400961010     D  STANRS                 8      9  0
009500961010     D  STANSP                10     16  0
009600961010     D  STASPE                 1     16
009700961010     D  STASTS                17     19
009800961010     D  STAERR                20     22
009900961010     D*  DS x rottura livello spedizione - codice errore
010000961010     D SAVSPE          DS
010100040305     D  CNI_WSV                3     16  0
010200961010     D  WSVAAS                 1      4  0
010300961010     D  WSVLNP                 5      7  0
010400961010     D  WSVNRS                 8      9  0
010500961010     D  WSVNSP                10     16  0
010600961015     D  WSVSPE                 1     16
010700961010     D  WSVSTS                17     19
010800961010     D  WSVERR                20     22
010900960528     D WLBDA8          DS
011000960528     D  G02DAT                 1      8  0
011100960528     D  G02INV                 9     16  0
011200960528     D  G02ERR                17     17
011300960528     D  G02TGI                18     22  0
011400110630      *----------------------------------------------------*
011500110630      *  Invio E-mail
011600110630     D Indirizzi       s            253
011700110630     D Oggetto         s             44
011800110630     D Messaggio       s           5000
011900110630      *
012000110630     d   DSmail        ds
012100110630     D Mail_Ind                      44
012200110630     d   IND_char                     1    dim(44)
012300110630      *
012400110630     D Lunghezza_Indirizzo...
012500110630     D                 s              5u 0
012600110630     C*---------------------------------------------------------------*
012700110630     D MsgDSP          S            256                                         Testo generico
012800110630     C*---------------------------------------------------------------*
012900110630     D SndMg01         S             10                                         Message type
013000110630     D                                     INZ('*INFO')
013100110630     D SndMg02         S             10                                         Deluvery mode
013200110630     D                                     INZ('*BREAK')
013300110630     D SndMg03         S            256                                         Message text
013400110630     D SndMg04         S             10I 0                                      Length of text
013500110630     D                                     INZ(%SIZE(SndMg03))
013600110630     D SndMg05         S             10                                         User profile
013700110630     D                                     DIM(299)
013800110630     D SndMg06         S             10I 0                                      Number of user
013900110630     D SndMg07         S             10I 0                                      Message sent indic.
014000110630     D SndMg08         S             10I 0                                      Function requested
014100110630     D SndMg10         S              1                                         Show display
014200110630     D                                     INZ('N')
014300110630     D SndMg11         S             20                                         Qualified MSGQ name
014400110630     D SndMg12         S              4                                         Name type indicator
014500110630     D                                     INZ('*USR')
014600110630      * indice di scihera
014700110630     D Jx              S              5I 0
014800110630      *
014900110630     D/COPY QSYSINC/QRPGLESRC,QUSEC
015000110630      *
015100120905     d bart_it         C                   CONST('@Brt.it;')
015200120905     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
015300110630
015400960708     D*
015500980421     D COST1           C                   CONST('IT/BAR/IT1   ')
015600110330     D CALB_Plat       C                   CONST('039IT04507990150')
015700960528     I*---------------------------------------------------------------------*
015800960528     I* FILE
015900960528     I*---------------------------------------------------------------------*
016000960528     IIFTSTA00
016100960528     I              SSIFTSTA                    STADAT
016200960528     C*---------------------------------------------------------------------*
016300960528     C* Ciclo principale
016400960528     C*---------------------------------------------------------------------*
016500960528      * Posizionamento iniziale sul file  (prima lettura)
016600961010     C                   EXSR      POSFIL
016700961010      *
016800961010      * Loop letture fino a fine file EDSTA
016900961010     C     *IN30         DOWEQ     '0'
017000961010      * Se ho già scritto 999 spedizioni devo scrivere un nuovo msg
017100961010     C* Caricamento NUMERATORE
017200961010     C                   EXSR      CARNUM
017300961010      * Eseguo scrittura dati record ST00
017400961010     C                   EXSR      WRST00
017500961010      * Eseguo scrittura dati record ST05
017600961010     C                   EXSR      WRST05
017700961010      * Eseguo scrittura dati record ST10
017800961010     C                   EXSR      WRST10
017900961010      * Eseguo scrittura dati record ST15
018000961010     C                   EXSR      WRST15
018100961010      *
018200961010      * Se cambia il nr.CMR o se ho già scritto 999 bolle scrivo
018300961010      * intestazione nuovo messaggio
018400961010     C     STACMR        DOWEQ     WSVCMR
018500961010     C     WPROG         ANDLT     999
018600961010     C     *IN30         ANDEQ     '0'
018700961010      * Salvataggio dati spedizione - errore
018800961010     C                   EXSR      SAVDAT
018900961010      *
019000961010      * Loop lettura bolla x nr.spedizione codice errore
019100961010     C     SPEERR        DOWEQ     SAVSPE
019200961010     C     STACMR        ANDEQ     WSVCMR
019300961010     C     *IN30         ANDEQ     '0'
019400961010     C                   EXSR      GESBOL
019500961010      * Lettura file
019600961010     C                   EXSR      REDFIL
019700961010     C                   END
019800961010      *
019900960528      * Eseguo scrittura dati record SD00
020000961015     C     WSVSTS        IFEQ      *BLANKS
020100961015     C     WSVERR        ANDEQ     *BLANKS
020200961015     C     §PUDTB        ANDEQ     'E'
020300961015     C     WSVSTS        OREQ      *BLANKS
020400961015     C     WSVERR        ANDEQ     *BLANKS
020500961015     C     §PUSGD        ANDEQ     'S'
020600961015     C     WSVSTS        ORNE      *BLANKS
020700961015     C     WSVERR        ORNE      *BLANKS
020800960528     C                   EXSR      WRSD00
020900960528      * Eseguo scrittura dati record SD05
021000110330     C                   MOVEL     'N'           WRTEVE            1
021100960528     C                   EXSR      WRSD05
021200110330      * Scrivo tipi record successivi solo se ho scritto SD00 e SD05
021300110330     C     WRTEVE        IFEQ      'S'
021400960528      * Eseguo scrittura dati record SD10
021500960528     C                   EXSR      WRSD10
021600960528      * Eseguo scrittura dati record SD15
021700960528     C                   EXSR      WRSD15
021800960528      * Eseguo scrittura dati record SD20
021900960528     C                   EXSR      WRSD20
022000110330     C                   ENDif
022100961015     C                   END
022200961015     C*
022300960528     C                   END
022400961010     C                   END
022500110418      * Chiusura messaggio
022600110418     C                   EXSR      end_MSG
022700110418      *
022800010409      *  PRIMA DI USCIRE IMPOSA IL TIPO DI TRASMISSIONE (DA TAB."PT")
022900010409      *   VIA INTESA O VIA FTP DA SEDE UTILIZZANDO L'ULTIMO BYTE
023000010409      *     A DESTRA DELLA KPJBU POICHÈ NELLA KPJBU DA SINISTRA CI
023100010409      *      SONO LE ALTRE INFORMAZIONI RELATIVE ALLA DS TRTC78DS.
023200010410     C                   MOVEL     *BLANK        MEMBR0           10
023300010410     C                   MOVEL     'M'           MEMBR8            8
023400010410     C                   MOVE      §PTKSC        MEMBR8
023500010410     C                   MOVEL     MEMBR8        MEMBR0
023600010410     C                   MOVEL     MEMBR0        FLD11            11
023700010410     C                   MOVE      §PTFTT        FLD11            11
023800010410     C                   MOVE      FLD11         KPJBU
023900960528     C* Fine pgm.
024000121010      *   se si tratta di un traduttore NON EDIFACT deve far saltare al CL la parte
024100121010      *    successiva di trasmissione
024200121010     c                   if        §PZPGMTRSS = 'GELSTA'
024300131007     c                                or
024400131007     c                             §PZPGMTRSS = 'AMZSTA'
024500121010     c                   eval      Righe_Dett = 'N'
024600121010     c                   end
024700121010      *
024800960528     C                   SETON                                        LR
024900961010     C*----------------------------------------------------------------
025000961010     C*? POSFIL - POSIZIONAMENTO PRIMO RECORD
025100961010     C*----------------------------------------------------------------
025200961010     C     POSFIL        BEGSR
025300961010     C*
025400961010     C* Mi posiziono su EDSTA con cod.cliente - nr.CMR.
025500980401     C     D87LNP        IFEQ      0
025600980401     C                   Z-ADD     D87CCM        KKEY1
025700980401     C                   ELSE
025800980401     C                   Z-ADD     D87LNP        KKEY1
025900980401     C                   END
026000961010     C                   Z-ADD     D87NFE        KNFE
026100961010     C     KSTA          CHAIN     EDSTAWRK                           30
026200110330      *   Se fine File
026300110330     C                   If        *IN30 = *on
026400110330     C                   MOVEL     'S'           WFINE             1
026500110330     C                   EndIF
026600110330      *
026700961010     C                   ENDSR
026800960528     C*----------------------------------------------------------------
026900960528     C*? REDFIL - LETTURA PROSSIMO RECORD
027000960528     C*----------------------------------------------------------------
027100960528     C     REDFIL        BEGSR
027200960528     C*
027300961010     C* Deleto vecchio record
027400961010     C                   DELETE    EDSTA000
027500961010     C* Lettura file EDSTA
027600961010     C     KSTA          READE     EDSTAWRK                               30
027700110330      *   Se fine File
027800110330     C                   If        *IN30 = *on
027900110330     C                   MOVEL     'S'           WFINE             1
028000110330     C                   EndIF
028100960528     C*
028200960528     C                   ENDSR
028300961010     C*----------------------------------------------------------------
028400110414     C*? chiusura Messaggio ULTIMI Segmenti da riportare
028500110414     C*----------------------------------------------------------------
028600110414     C     End_MSG       BEGSR
028700110414      *
028800110414      *   e deve essere tradotto NON CON INTESA....
028900110414     c                   If        §PZPGMTRSS = 'STA94A'
029000110414     C                               and  Numero_Righe > 1
029100120831     c                               or
029200120831     c                             §PZPGMTRSS = 'GELSTA'
029300120831     C                               and  Numero_Righe > 0
029400131007     c                               or
029500131007     c                             §PZPGMTRSS = 'AMZSTA'
029600131007     C                               and  Numero_Righe > 0
029700110414      *
029800110414      * Se c'è stato qualche evento da inviare che quindi ha scritto
029900110414      *   chiude il messaggio altrimenti esegue un ROLLBACK
030000110414     c                   if        Righe_Dett = 'S'
030100120831      *
030200110414      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
030300110414      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
030400110414     c                   move      §MSNUM        fine04            4
030500110414     c                   eval      STATPR = 'FINE'
030600110414     c                   eval      STADAT = fine04
030700120831      *
030800120831     c                   exsr      write_OUTPUT
030900110414     c                   eval      Righe_Dett = 'N'
031000110414     c                   else
031100120831      *
031200120831      * deve uscire senza avere fatto nulla
031300110414     c                   eval      STATPR = 'RLBK'
031400120831     c                   exsr      write_OUTPUT
031500110414      *
031600110414     c                   end
031700110414     c                   end
031800110414     C*
031900110414     C                   ENDSR
032000110414     C*----------------------------------------------------------------
032100961010     C*? SAVDAT - SALVATAGGIO DATI PER SCRITTURA BOLLA
032200961010     C*----------------------------------------------------------------
032300961010     C     SAVDAT        BEGSR
032400961010     C*
032500961010     C*  Mi imposto i dati spedizione dalla prima bolla letta
032600961010     C*  per poi eseguire scrittura dettaglio a rottura bolla
032700961010     C*  codice errore
032800961010     C                   MOVEL     SPEERR        SAVSPE
032900040305     C                   MOVEL     CNI_STA       CNI_WSV
033000961010     C                   MOVEL     STACNI        WSVCNI
033100980429     C                   MOVEL     STADTR        WSVDTR
033200980429     C                   MOVEL     STAHMS        WSVHMS
033300961010     C                   MOVEA     *BLANKS       SGN
033400961010     C                   MOVEL     *BLANKS       WDAT
033500961010     C                   Z-ADD     0             WNRCOL            5 0
033600961010     C                   Z-ADD     0             Y                 3 0
033700961010     C*
033800961010     C                   ENDSR
033900961010     C*----------------------------------------------------------------
034000961010     C*? GESBOL - Carico dati sgn
034100961010     C*----------------------------------------------------------------
034200961010     C     GESBOL        BEGSR
034300961010     C*
034400961010     C                   ADD       1             WNRCOL
034500961010     C*  Eseguo caricamento det.colli se devo inviarli
034600961010     C     §PUSGD        IFEQ      'S'                                          sgn sempre
034700961010     C     §PUSGD        OREQ      'N'                                          sgn negat.
034800980402     C     STAFLE        ANDNE     *BLANKS                                      errore
034900980330     C* Controllo se devo tornare segnacollo euroexpress
035000980330     C     §PUBS1        IFEQ      'E'
035100980330     C                   EXSR      CARSGE
035200980330     C                   ELSE
035300961010     C* Calcolo nr.segnacollo da passare al cliente
035400961015     C                   EXSR      CARSGN
035500961010     C                   ADD       1             Y
035600961010     C                   MOVEL     WSGN          SGN(Y)
035700961010     C                   END
035800980330     C                   END
035900961010     C*
036000961010     C                   ENDSR
036100960528     C*----------------------------------------------------------------
036200960528     C*? WRST00 - Scrittura record partner
036300960528     C*----------------------------------------------------------------
036400960528     C     WRST00        BEGSR
036500960528     C*
036600960528     C*  Pulisco dati DS
036700960528     C                   CLEAR                   EDST00
036800960528     C*  Mittente: Bartolini  (ITBAR + AS..)
036900960528     C                   MOVEL     'ITBAR'       WMITT             8
037000961002     C                   MOVE      WAS400        WMITT             8
037100960530     C                   MOVEL     WMITT         TA0004
037200960729     C                   MOVEL     'ZZ'          TA0007
037300961010     C                   MOVEL     D87CPT        TA0010
037400961029     C                   MOVE      '    '        TA0010
037500960827     C*  Utilizzo qualificatore x destinatario msg.
037600960827     C                   MOVEL     §PTMSQ        TAA007
037700960528     C*  Data/Ora invio messagio: adesso
037800980109     C                   MOVEL     WOGGI         TA0017
037900960528     C                   MOVEL     WHHMM         TA0019
038000960528     C*  Codice documento - nome documento
038100961002     C                   MOVEL     '781'         TA1001
038200961002     C                   MOVEL     D87NFE        TA1004                         Nr.F.entrata
038300960528     C                   MOVEL     '9  '         TA1225
038400961010     C*  Data foglio entrata
038500961010     C                   MOVEL     '137'         TA2005
038600961010     C                   MOVEL     D87DFE        TA2380                         Data F.E.
038700960528     C                   MOVEL     '102'         TA2379
038800100512      *
038900100512      *    Se si vuole la data con l'orario
039000100512     c                   if        §pudate203 ='S'
039100100512     C                   MOVE      Whhmm         TA2380
039200100512     C                   MOVEL(p)  '203'         TA2379
039300100512     c                   end
039400100512      *
039500960528     C*  Scivo record IFTSTA
039600960705     C                   CLEAR                   WDAT
039700960705     C                   MOVEL     EDST00        WDAT
039800960726     C                   Z-ADD     §MSNUM        STAPRG
039900960726     C                   MOVEL     'ST00'        STATPR
040000960705     C                   MOVEL     WDAT          STADAT
040100110330      *
040200120831      *  Testata messaggio  UNB-BGM-DTM
040300120831     c                   exsr      write_OUTPUT
040400110330      *
040500960528     C                   ENDSR
040600960708     C*----------------------------------------------------------------
040700960708     C*? WRST05 - Scrittura record partner
040800960708     C*----------------------------------------------------------------
040900960708     C     WRST05        BEGSR
041000960708     C*
041100960708     C                   CLEAR                   EDST05
041200960708     C*  Ship from:
041300960712     C                   MOVEL     *BLANKS       WDAT
041400960708     C                   MOVEL     'SF '         TE3035
041500980421     C                   MOVEL     §PTPLT        TE3036
041600960708     C*  Scivo record IFCSUM
041700960708     C                   CLEAR                   WDAT
041800960708     C                   MOVEL     EDST05        WDAT
041900960726     C                   Z-ADD     §MSNUM        STAPRG
042000960712     C                   MOVEL     'ST05'        STATPR
042100960708     C                   MOVEL     WDAT          STADAT
042200110330      *
042300120831      *  Testata messaggio  NAD-SF
042400120831     c                   exsr      write_OUTPUT
042500960708     C*  Ship to:
042600960712     C                   MOVEL     *BLANKS       WDAT
042700960708     C                   CLEAR                   EDST05
042800960708     C                   MOVEL     'ST '         TE3035
042900980421     C                   MOVEL     COST1         TE3036
043000110330      *
043100110330      *  x CALBERSON --> GEODIS
043200110330      *      imposta la Platform che ci hanno chiesto di utilizzare
043300110330      *      x identificare diversamente dai dati di DUNLOP altrimenti
043400110330      *      non riescono a dividerli nel loro sistema.
043500110330      *  da impostare il §PTLOC ??????
043600110330     c                   if        §PTKSC = 493298
043700110330     C                   clear                   TE3036
043800110330     C                   MOVEL     CALB_Plat     TE3036
043900110330     c                   end
044000110330      *
044100960708     C*  Scivo record IFCSUM
044200960708     C                   CLEAR                   WDAT
044300960708     C                   MOVEL     EDST05        WDAT
044400960726     C                   Z-ADD     §MSNUM        STAPRG
044500960712     C                   MOVEL     'ST05'        STATPR
044600960708     C                   MOVEL     WDAT          STADAT
044700110330      *
044800110330      *  Testata messaggio  NAD-ST
044900120831     c                   exsr      write_OUTPUT
045000960708     C*
045100960708     C                   ENDSR
045200960528     C*----------------------------------------------------------------
045300960528     C*? WRST10 - Scrittura record partner
045400960528     C*----------------------------------------------------------------
045500960528     C     WRST10        BEGSR
045600960528     C*
045700960528     C*  Pulisco dati DS
045800110330     C                   MOVEL     STACMR        WSVCMR
045900980402     C     §PTCM1        IFEQ      'S'
046000980402     C     §PTNF1        OREQ      'S'
046100960712     C                   MOVEL     *BLANKS       WDAT
046200960528     C                   CLEAR                   EDST10
046300960726     C*  Se devo ritornare imposto AFB + nr.messaggio
046400961010     C                   MOVEL     STATPD        TB1153
046500961010     C                   MOVEL     STACMR        TB1154
046600961010     C                   MOVEL     STACMR        WSVCMR
046700960726     C*  Scivo record IFCSUM
046800960726     C                   CLEAR                   WDAT
046900960726     C                   MOVEL     EDST10        WDAT
047000960726     C                   Z-ADD     §MSNUM        STAPRG
047100960726     C                   MOVEL     'ST10'        STATPR
047200960726     C                   MOVEL     WDAT          STADAT
047300110330      *
047400120831      *  Testata messaggio  RFF-LOC
047500120831     c                   exsr      write_OUTPUT
047600110330      *
047700980402     C                   END
047800960528     C*
047900960528     C                   ENDSR
048000960528     C*----------------------------------------------------------------
048100960528     C*? WRST15 - Scrittura record partner
048200960528     C*----------------------------------------------------------------
048300960528     C     WRST15        BEGSR
048400960528     C*
048500960528     C*  Pulisco dati DS
048600960712     C                   MOVEL     *BLANKS       WDAT
048700960528     C                   CLEAR                   EDST15
048800960528     C*
048900960528     C                   ENDSR
049000960528     C*----------------------------------------------------------------
049100960528     C*? WRSD00 - Scrittura record partner - dettaglio
049200960528     C*----------------------------------------------------------------
049300960528     C     WRSD00        BEGSR
049400960528     C*
049500960528     C*  Pulisco dati DS
049600960712     C                   MOVEL     *BLANKS       WDAT
049700960528     C                   CLEAR                   EDSD00
049800120705      * Default
049900120705      *  se non richiesta l'inversione del dato CNI nell'AGE
050000120705     c                   if        §ptTRD <> 'S'
050100120705      *
050200960528     C*  Imposto numero spedizione cliente
050300040305     C                   MOVEL     CNI_WSV       DA1004
050400120705      *
050500120705      *  inverte con l'AGE
050600120705     c                   elseIF    §ptTRD  = 'S'
050700120705     C                   MOVEL(p)  WSVCNI        DA1004
050800120705     c                   end
050900120705      *
051000961010      * se non è dischetto lo segnalo
051100961015     C   01
051200961015     CANN01WSVCMR        IFEQ      *BLANKS
051300960821     C                   MOVEL     'S'           WSD15             1
051400961015     C                   ENDIF
051500961010     C*  Incremento proggr. a rottura nr.spedizione
051600961015     C     WSVSPE        IFNE      WEXSPE
051700961015     C                   MOVEL     WSVSPE        WEXSPE           16
051800961010     C                   END
051900060406      ** Deve sempre incrementare
052000060406     C                   ADD       1             WPROG
052100961010     C                   MOVEL     WPROG         DA1490
052200961010     C                   MOVEL     '9999'        DA1312
052300961010     C*  Scivo record IFTSTA
052400961010     C                   CLEAR                   WDAT
052500961010     C                   MOVEL     EDSD00        WDAT
052600961010     C                   Z-ADD     §MSNUM        STAPRG
052700961010     C                   MOVEL     'SD00'        STATPR
052800961010     C                   MOVEL     WDAT          STADAT
052900110330      *
053000120831      *  Testata messaggio  CNI-
053100120831     c                   exsr      write_OUTPUT
053200960528     C*
053300960528     C                   ENDSR
053400960528     C*----------------------------------------------------------------
053500960528     C*? WRSD05 - Scrittura record partner - dettaglio
053600960528     C*----------------------------------------------------------------
053700960528     C     WRSD05        BEGSR
053800960528     C*
053900960528     C*  Pulisco dati DS
054000960712     C                   MOVEL     *BLANKS       WDAT
054100960528     C                   CLEAR                   EDSD05
054200961010     C                   MOVEL     'AGE'         DB1153
054300120705      * Default
054400120705      *  se non richiesta l'inversione del dato CNI nell'AGE
054500120705     c                   if        §ptTRD <> 'S'
054600961010     C                   MOVEL     WSVCNI        DB1154
054700120705      *  inverte con il CNI
054800120705     c                   elseIF    §ptTRD  = 'S'
054900120705     C                   MOVEL     CNI_WSV       DB1154
055000120705     c                   end
055100120705      *
055200961015     C                   MOVEL     WSVSTS        DB9011
055300961015     C                   MOVEL     WSVERR        DB9013
055400960528     C                   MOVEL     '7  '         DB2005
055500961010     C*  Controllo se ho anche l'ora o solo la data
055600980429     C     WSVHMS        IFEQ      *ZEROS
055700980429     C                   MOVEL     WSVDTR        DB2380
055800960528     C                   MOVEL     '102'         DB2379
055900100512      *
056000100512      *    Se si vuole la data con l'orario
056100100512     c                   if        §pudate203 ='S'
056200100512     C                   MOVE      '0001'        DB2380
056300100512     C                   MOVEL(p)  '203'         DB2379
056400100512     c                   end
056500100512      *
056600961010     C                   ELSE
056700980429     C                   MOVEL     WSVDTR        WDATHH           12
056800980429     C                   MOVE      WSVHMS        WDATHH
056900961010     C                   MOVEL     WDATHH        DB2380
057000961010     C                   MOVEL     '203'         DB2379
057100961010     C                   END
057200960917     C*  Scivo record IFTSTA
057300960917     C                   CLEAR                   WDAT
057400961010     C                   MOVEL     EDSD05        WDAT
057500960917     C                   Z-ADD     §MSNUM        STAPRG
057600961010     C                   MOVEL     'SD05'        STATPR
057700960917     C                   MOVEL     WDAT          STADAT
057800110330      *
057900120831      *  Testata messaggio  STS-RFF-DTM
058000120831     c                   exsr      write_OUTPUT
058100110330      *
058200110330     C                   MOVEL     'S'           WRTEVE            1
058300110330     c                   eval      Righe_Dett = 'S'
058400110330      *
058500960528     C     FIND05        ENDSR
058600960528     C*-------------------------------------------------------------*
058700960528     C*? WRSD10 - Scrittura record partner - dettaglio
058800960528     C*----------------------------------------------------------------
058900960528     C     WRSD10        BEGSR
059000960528     C*
059100960528     C*  Pulisco dati DS
059200961010     C                   MOVEL     *BLANKS       WDAT
059300960528     C                   CLEAR                   EDSD10
059400960528     C*
059500960528     C                   ENDSR
059600960528     C*-------------------------------------------------------------*
059700960528     C*? WRSD15 - Scrittura record partner - dettaglio
059800960528     C*----------------------------------------------------------------
059900960528     C     WRSD15        BEGSR
060000960528     C*
060100960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
060200960821     C     WSD15         IFEQ      'S'
060300960821     C                   MOVEL     *BLANKS       WDAT
060400960821     C                   CLEAR                   EDSD15
060500960821     C                   MOVEL     'AAI'         DD4451
060600960821     C                   MOVEL     FTX(1)        DD4440
060700960821     C                   MOVEL     EDSD15        WDAT
060800960821     C                   Z-ADD     §MSNUM        STAPRG
060900960821     C                   MOVEL     'SD15'        STATPR
061000960821     C                   MOVEL     WDAT          STADAT
061100110330      *
061200120831      *  Testata messaggio  FTX-
061300120831     c                   exsr      write_OUTPUT
061400110330      *
061500110330     c                   eval      Righe_Dett = 'S'
061600960821     C                   CLEAR                   WSD15
061700960821     C                   END
061800961010     C*
061900960528     C                   ENDSR
062000960528     C*-------------------------------------------------------------*
062100960528     C*? WRSD20 - Scrittura record partner - dettaglio
062200960528     C*----------------------------------------------------------------
062300960528     C     WRSD20        BEGSR
062400960528     C*
062500960528     C*  Pulisco dati DS
062600960528     C                   CLEAR                   EDSD20
062700960528     C                   MOVEL     '99999'       DE1496
062800961010     C*  carico dettaglio segnacolli
062900960912     C                   EXSR      CARCOL
063000960528     C*
063100960528     C                   ENDSR
063200960912     C*----------------------------------------------------------------
063300960912     C*? CARCOL - Routine caricamento colli da dettag.
063400960912     C*----------------------------------------------------------------
063500960912     C     CARCOL        BEGSR
063600960912     C*
063700961010     C*  Scrivo D20 da SGN
063800961029     C     Y             IFNE      0
063900961010     C                   MOVE      Y             DE7224
064000961015     C                   MOVEL     'PK'          DE7065
064100960912     C                   Z-ADD     0             WW                3 0
064200040218     C                   Z-ADD     0             WE                3 0
064300040217      *  imposta quante volte deve eseguire il ciclo per quanti segnacolli
064400040217      *  imposta per ogni record.
064500050428     c                   if        §Puel2 = 0
064600050428     c                   eval      §Puel2 = 1
064700050428     c                   end
064800050428      *
064900040217     c     y             div       §Puel2        volte
065000040217     c                   mvr                     resto             3 0
065100040217     c                   if        resto > 0
065200040217     c                   add       1             volte
065300040217     c                   end
065400040217      *
065500040217     C                   DO        volte         vl                3 0
065600040218      * Impacchetta sulla schiera di 20 elementi quanti elementi servono
065700040218     C                   DO        §PUEL2        WW
065800040218     c                   add       1             WE
065900040218     C                   MOVEL     SGN(WE)       D20(WW)
066000040218     c                   ENDDO
066100040218      *
066200980424     C     D20(1)        IFNE      *BLANKS
066300960913     C                   MOVEL     '24'          DE4233
066400980424     C                   ELSE
066500980424     C                   MOVEL     *BLANKS       DE4233
066600980424     C                   MOVEA     *BLANKS       D20
066700980424     C                   END
066800040218      *
066900960912     C                   EXSR      WRT20
067000040218      *
067100960912     C*  Se ci sono ancora dei colli clear tracciato
067200960912     C*  e scrittura nuovo record
067300960912     C                   MOVEL     *BLANKS       EDSD20
067400960912     C                   END
067500961029     C*
067600961029     C                   ELSE
067700961029     C                   CLEAR                   EDSD20
067800961104     C                   MOVEL     '99999'       DE1496
067900961029     C                   MOVE      WNRCOL        DE7224
068000961029     C                   MOVEL     'PK'          DE7065
068100961029     C                   EXSR      WRT20
068200961029     C                   END
068300960912     C*
068400960912     C                   ENDSR
068500960912     C*----------------------------------------------------------------
068600960912     C*? WRT20 - scrivo record d20
068700960912     C*----------------------------------------------------------------
068800960912     C     WRT20         BEGSR
068900960912     C*
069000960912     C*  Scrivo record SD20
069100051202     C*    ma solo se contiene delle informazioni altrimenti salto
069200960912     C                   MOVEL     EDSD20        WDAT
069300051202     C*
069400051202     c                   if        %subst(WDAT:10:1900) <> *blanks
069500960912     C                   MOVEL     'SD20'        STATPR
069600960912     C                   Z-ADD     §MSNUM        STAPRG
069700960912     C                   MOVEL     WDAT          STADAT
069800110330      *
069900120831      *  Testata messaggio  GID-PCI
070000120831     c                   exsr      write_OUTPUT
070100110330     c                   eval      Righe_Dett = 'S'
070200051202     c                   end
070300960912     C*
070400960912     C                   ENDSR
070500960730     C*----------------------------------------------------------------
070600960912     C*? CARSGN - caricamneto segnacollo
070700960730     C*----------------------------------------------------------------
070800960912     C     CARSGN        BEGSR
070900960730     C*
071000960730     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
071100960912     C                   MOVEA     *BLANKS       C35
071200960912     C                   MOVEL     *BLANKS       WSGN             35
071300960912     C*  Compongo lnp se la devo passare
071400960912     C     §PULP2        IFNE      0
071500960912     C                   Z-ADD     §PULP2        WA                3 0
071600961029     C                   MOVEL     STAFLS        WLNP              3
071700960912     C                   MOVEA     WLNP          C35(WA)
071800960912     C                   END
071900960912     C*  Compongo lna se la devo passare
072000960912     C     §PULA2        IFNE      0
072100960912     C                   Z-ADD     §PULA2        WA                3 0
072200961010     C                   MOVEL     STALNA        WLNA              3
072300960912     C                   MOVEA     WLNA          C35(WA)
072400960912     C                   END
072500960912     C*  Compongo nrs se lo devo passare
072600960912     C     §PUNR2        IFNE      0
072700960912     C                   Z-ADD     §PUNR2        WA                3 0
072800961010     C                   MOVEL     STANRS        WNRS              2
072900960912     C                   MOVEA     WNRS          C35(WA)
073000960912     C                   END
073100960912     C*  Compongo nsc se la devo passare
073200960912     C     §PUSG2        IFNE      0
073300960912     C                   Z-ADD     §PUSG2        WA                3 0
073400961010     C                   MOVEL     STANSC        WNSC              7
073500960912     C                   MOVEA     WNSC          C35(WA)
073600960912     C                   END
073700960912     C*  Compongo znc se la devo passare
073800960912     C     §PUZN2        IFNE      0
073900960912     C                   Z-ADD     §PUZN2        WA                3 0
074000970626     C                   MOVE      STAZNC        WZNC              2
074100960912     C                   MOVEA     WZNC          C35(WA)
074200960912     C                   END
074300960912     C*  metto C35 in WSGN
074400960912     C                   MOVEA     C35           WSGN
074500960730     C*
074600960730     C                   ENDSR
074700980330     C*----------------------------------------------------------------
074800980330     C*? CARSGE - caricamneto segnacollo Euroexpress
074900980330     C*----------------------------------------------------------------
075000980330     C     CARSGE        BEGSR
075100980330     C*
075200980401     C* Controllo se ho aggiunto eccedenze euroexpress
075300980401     C     STANSC        IFEQ      *ZEROS
075400980401     C                   MOVEL     *BLANKS       WSVCNI
075500980402     C                   MOVEL     '0'           WSVCNI
075600980401     C                   Z-ADD     1             Y
075700980401     C                   MOVEL     STACNI        SGN(Y)
075800980401     C* Abblenco SAVSPE
075900980401     C                   MOVEL     *BLANKS       SAVSPE
076000980402     C                   MOVEL     *LOVAL        WEXSPE
076100980401     C                   ELSE
076200980401     C* Aggancio segnacollo eroexpress
076300980401     C                   Z-ADD     STAFLS        KFLS
076400980401     C                   Z-ADD     STALNA        KLNA
076500980401     C                   Z-ADD     STANRS        KNRS
076600980401     C                   Z-ADD     STANSC        KNSC
076700980330     C                   MOVEL     'E'           KTRC
076800051017     C                   CLEAR                   FIARS000
076900051017     C     KARS          CHAIN     FIARS01L                           33
077000061031      *
077100061031      *  Se si tratta di un cliente che riceve i dati non IFCSUM ma da VAB
077200061031      *  i tipi records su ARS sono classificati come "C" diskC
077300061031     C   33              MOVEL     'C'           KTRC
077400061031     C   33KARS          CHAIN     FIARS01L                           33
077500980330     C                   ADD       1             Y
077600051017     C  N33              MOVEL     ARSNOT        SGN(Y)
077700980401     C                   END
077800980330     C*
077900980330     C                   ENDSR
078000960927     C*----------------------------------------------------------------
078100960927     C*? CARNUM - CARICAMENTO NUMERATORE
078200960927     C*----------------------------------------------------------------
078300960927     C     CARNUM        BEGSR
078400960927     C*
078500960927     C                   Z-ADD     0             WPROG             4 0
078600960927     C                   Z-ADD     0             X                 3 0
078700960927     C                   Z-ADD     1             §MSNUM
078800961010     C                   MOVEL     'MS'          KCOD
078900961010     C                   MOVEL     *BLANKS       KKEY
079000961010     C                   MOVEL     '781'         KKEY
079100961010     C     KTAB          CHAIN     EDTAB01L                           30
079200960927     C     *IN30         IFEQ      '0'
079300960927     C     TABFLG        ANDEQ     *BLANKS
079400960927     C                   MOVEL     TABUNI        DSMS
079500960927     C                   ADD       1             §MSNUM
079600061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
079700061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
079800061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
079900061219     c                   move      §MSNUM        campo_4n          4 0
080000061219     c                   if        campo_4n = 0
080100061219     c                   add       1             §MSNUM
080200061219     c                   end
080300960927     C                   Z-ADD     WOGGI         §MSDAT
080400960927     C                   MOVEL     DSMS          TABUNI
080500960927     C                   UPDATE    EDTAB000
080600960927     C                   END
080700960927     C*
080800960927     C                   ENDSR
080900960528     C*----------------------------------------------------------------
081000960528     C*? *INZSR - OPERAZIONI INIZIALI
081100960528     C*----------------------------------------------------------------
081200960528     C     *INZSR        BEGSR
081300960530     C*
081400960530     C     *ENTRY        PLIST
081500960530     C                   PARM                    KPJBA
081600110630     C                   PARM                    Righe_Dett
081700110630     C                   PARM                    Esito             1            ritorno errore
081800110630      *
081900110630      * reset campo compattazione x Download file
082000110630     C                   eval      esito = '0'
082100110630     c                   eval      Compatta_80 = 'S'
082200110704     c                   eval      Righe_Dett = 'N'
082300110601      *
082400961010     C                   MOVEL     KPJBU         TRTC87
082500110601      *
082600960528     C* Definisco chiavi di accesso
082700961010     C     KTAB          KLIST
082800961010     C                   KFLD                    KCOD
082900961010     C                   KFLD                    KKEY
083000120831      *
083100961010     C     KSTA          KLIST
083200980401     C                   KFLD                    KKEY1
083300961010     C                   KFLD                    KNFE
083400120831      *
083500051017     C     KARS          KLIST
083600980330     C                   KFLD                    KFLS
083700980330     C                   KFLD                    KLNA
083800980330     C                   KFLD                    KNRS
083900980330     C                   KFLD                    KNSC
084000980330     C                   KFLD                    KTRC
084100120831      *
084200120831      * DEVE ESSERE PILOTATO DALLA TAB.3K
084300120831     C     KTAB2         KLIST
084400120831     C                   KFLD                    TBLKUT
084500120831     C                   KFLD                    TBLCOD
084600120831     C                   KFLD                    TBLKEY
084700120831      *
084800960528     C* Definizione campi di work
084900051017     C     *LIKE         DEFINE    ARSFLS        KFLS
085000051017     C     *LIKE         DEFINE    arSLNA        KLNA
085100051017     C     *LIKE         DEFINE    arSNRS        KNRS
085200051017     C     *LIKE         DEFINE    arSNSC        KNSC
085300051017     C     *LIKE         DEFINE    arSTRC        KTRC
085400980330     C     *LIKE         DEFINE    TABCOD        KCOD
085500961010     C     *LIKE         DEFINE    TABKEY        KKEY
085600980401     C     *LIKE         DEFINE    STAKEY        KKEY1
085700961010     C     *LIKE         DEFINE    STANFE        KNFE
085800961010     C     *LIKE         DEFINE    STACMR        WSVCMR
085900961010     C     *LIKE         DEFINE    STACNI        WSVCNI
086000980429     C     *LIKE         DEFINE    STADTR        WSVDTR
086100980429     C     *LIKE         DEFINE    STAHMS        WSVHMS
086200040217     C     *LIKE         DEFINE    y             volte
086300960528     C*
086400960528     C* Recupero codice AS
086500960528     C                   Z-ADD     1             CODUT             1 0
086600960705     C                   CALL      'X§PARUT'
086700960528     C                   PARM                    UTEDSE
086800960528     C                   MOVEL     REC80         CNCR80
086900980622     C                   MOVEL     '005'         WAS400            3
087000960528     C* Recupero data e ora x messaggio
087100960528     C                   TIME                    WHHDAT           14 0
087200960708     C                   MOVE      WHHDAT        G02DAT
087300960528     C                   MOVE      WHHDAT        DATA              6 0
087400960528     C                   MOVEL     *BLANK        G02ERR
087500960528     C                   CALL      'XSRDA8'
087600960528     C                   PARM                    WLBDA8
087700960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
087800960708     C                   MOVEL     WHHDAT        WHHMM             4 0
087900960528     C                   MOVE      WOGGI         WOGGI6            6 0
088000960528     C                   MOVE      WHHDAT        WANNO             2 0
088100960528     C                   MOVEL     WANNO         WOGGI6
088200960528     C* Caricamento Tabella Partner esteri
088300960528     C                   Z-ADD     0             X                 3 0
088400961015     C                   MOVEL     'PT'          KCOD
088500961015     C                   MOVEL     D87CPT        KKEY
088600120831     c                   clear                   dsPT
088700961010     C     KTAB          CHAIN     EDTAB01L                           30
088800961010     C     *IN30         IFEQ      '0'
088900961010     C     TABFLG        ANDEQ     *BLANKS
089000961010     C                   MOVEL     TABUNI        DSPT
089100960528     C                   END
089200961015     C                   MOVEL     'PU'          KCOD
089300120831     c                   clear                   dsPU
089400961010     C     KTAB          CHAIN     EDTAB01L                           30
089500961010     C     *IN30         IFEQ      '0'
089600961010     C     TABFLG        ANDEQ     ' '
089700961010     C                   MOVEL     TABUNI        DSPU
089800960912     C                   END
089900110330      *   Con quale traduttore deve essere scaricato e scritto il File ????
090000110330     C                   MOVEL     'PZ'          KCOD
090100120831     c                   clear                   dsPZ
090200110330     C     KTAB          CHAIN     EDTAB01L
090300110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
090400110330     C                   MOVEL     TABUNI        DSPZ
090500110330     C                   END
090600120831      *
090700120831     c                   eval      CLIENTE =  '0' + %editc(§PTKSC:'X')
090800120831     C                   Z-ADD     1             TBLKUT
090900120831     C                   MOVEL     '3K'          TBLCOD
091000120831     C                   moveL(P)  §PTKSC        TBLKEY
091100120831     C     KTAB2         chain     tabel00f
091200120831     c                   if        %Found(tabel00f)
091300120831     c                   eval      ds3K = tblUNI
091400120831     c                   eval      CLIENTE =  '0' + %editc(§3Kcks:'X')
091500120831     C                   END
091600960528     C* finta read x IFTSTA
091700960528     C   01
091800110407     CANN01              READ      EDSTA00F                               01
091900960528     C*
092000960528     C                   ENDSR
092100120831      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
092200120831      *  Scrive l'OUTPUT da inviare
092300120831      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
092400120831     C     write_OUTPUT  BEGSR
092500120831      *
092600120831      * Scrittura file per 20/210 Appuntamento da ATR
092700120831     c                   if        STATPR <> 'FINE' and
092800120831     c                             STATPR <> 'RLBK'
092900120831     C                   WRITE     IFTSTA00
093000120831     c                   end
093100120831      *
093200120831     c                   If        §PZPGMTRSS = 'STA94A'
093300120831     c                   exsr      Exit_segmenti
093400120831     c                   elseIf    §PZPGMTRSS = 'GELSTA'
093500120831     c                   exsr      Exit_FileGEL
093600131007     c                   elseIf    §PZPGMTRSS = 'AMZSTA'
093700131007     c                   exsr      Exit_FileAMZ
093800120831     C                   EnD
093900120831      *
094000120831     C                   ENDSR
094100110330      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
094200110330      *  Scrittura dei segmenti IFTSTA D94A
094300110330      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
094400110330     C     Exit_Segmenti BEGSR
094500120413      *
094600110330     c                   eval      Tipo_DS = STATPR
094700110330     c                   eval      Suffisso   = §PUSIG
094800110601     c                   eval      Compatta_80 = §pucompat
094900110330     c                   clear                   DsGenerica
095000110330     c                   eval      DsGenerica = STADAT
095100110330     c                   call      'TRTCT81R1'
095200110330     c                   PARM                    CLIENTE           8
095300110330     c                   PARM                    Suffisso          2
095400110330     c                   PARM                    Tipo_DS           4
095500110330     c                   PARM                    DsGenerica     1024
095600110330     C                   PARM                    Numero_Righe      5 0
095700110601     c                   PARM                    Compatta_80       1
095800110617     C                   PARM                    Progressivo      10
095900110621     c                   PARM                    esito             1
096000110630     C
096100110630     C                   if        esito = 'E'
096200110630     c                   exsr      Invio_Mail
096300110630     c                   end
096400110330      *
096500110330     C                   ENDSR
096600120831     c*==================================================================*
096700120831      * Manda un File a tracciato NETEXPRESS EUROPE
096800120831     c*==================================================================*
096900120831     c     Exit_FileGEL  begsr
097000120831      *
097100120831     c                   clear                   DsGenerica
097200120831     c                   eval      Tipo_DS = STATPR
097300120831     c                   eval      DsGenerica = STADAT
097400120905     c                   call      'TRTCT81GR'
097500120831     c                   PARM                    CLIENTE
097600120831     c                   PARM                    Tipo_DS           4
097700120831     c                   PARM                    DsGenerica     1024
097800120831     C                   PARM                    Numero_Righe      5 0
097900120831     C                   PARM                    Progressivo      10
098000120831     c                   PARM                    esito             1
098100120831     C
098200120831     C                   if        esito = 'E'
098300120831     c                   exsr      Invio_Mail
098400120831     c                   end
098500120831      *
098600120831     C                   ENDSR
098700131007     c*==================================================================*
098800131007      * Manda un File a tracciato IFTSTA Particolarissimo di AMAZON
098900131007     c*==================================================================*
099000131007     c     Exit_FileAMZ  begsr
099100131007      *
099200131007     c                   clear                   DsGenerica
099300131007     c                   eval      Tipo_DS = STATPR
099400131007     c                   eval      DsGenerica = STADAT
099500131007     c                   call      'TRTCT81AR'
099600131007     c                   PARM                    CLIENTE
099700131007     c                   PARM                    Tipo_DS           4
099800131007     c                   PARM                    DsGenerica     1024
099900131007     C                   PARM                    Numero_Righe      5 0
100000131007     C                   PARM                    Progressivo      10
100100131007     c                   PARM                    esito             1
100200131007     C
100300131007     C                   if        esito = 'E'
100400131007     c                   exsr      Invio_Mail
100500131007     c                   end
100600131007      *
100700131007     C                   ENDSR
100800110630     c*==================================================================*
100900110630      * Manda un Msg x E-mail
101000110630     c*==================================================================*
101100110630     c     Invio_Mail    begsr
101200110630      *
101300120831     c                   clear                   CLIENTEksc        8
101400120831     c                   eval      CLIENTEksc =  '0' + %editc(§PTKSC:'X')
101500130821???? C                   EVAL      Oggetto ='TRTCT88R DOWNLD ATR STATI EDI !!'
101600110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione ATR EDI-
101700110630     C                              STATI NON tradotto x invio su DOWNLD. -
101800110630     C                             Controllare il Download del Cliente. << '+
101900120831     c                             CLIENTEksc + ' >>'
102000110630      *
102100110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
102200110630     C* Inizializzo variabili
102300110630     C                   movel     *blanks       wrkEml          253
102400110630     C                   movel     *blanks       wrkMsg         5000
102500110630     C                   movel     *blanks       wrkOgg           44
102600110630      *
102700110630     C* Valorizzo i campi della e-m@ail - indirizzo
102800110630     C                   eval      wrkEml = CED_BART
102900110630     C                   eval      wrkOgg = Oggetto
103000110630     C                   eval      wrkMsg = Messaggio
103100110630     C                   call(e)   'TRTCT00R2'
103200110630     C                   parm                    wrkEml
103300110630     C                   parm                    wrkOgg
103400110630     C                   parm                    wrkMsg
103500110630     C*
103600110630     C                   ENDSR
103700110630      * ?------------------------------------------------------------------ */
103800110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
103900110630      * ?------------------------------------------------------------------ */
104000110630     C     *pssr         BEGSR
104100110630     C
104200110630     C                   eval      esito = 'E'
104300110630     c                   exsr      Invio_Mail
104400110630     C
104500110630     C                   ENDSR     '*CANCL'
104600110630     C
104700110630      *  ------------------------------------------------------------------ */
104800961010** FTX
104900961010RECEIVED REFERRING TO A SHIPMENT NOT RECEIVED WITH E.D.I. MESSAGE
