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
024300121010     c                   eval      Righe_Dett = 'N'
024400121010     c                   end
024500121010      *
024600960528     C                   SETON                                        LR
024700961010     C*----------------------------------------------------------------
024800961010     C*? POSFIL - POSIZIONAMENTO PRIMO RECORD
024900961010     C*----------------------------------------------------------------
025000961010     C     POSFIL        BEGSR
025100961010     C*
025200961010     C* Mi posiziono su EDSTA con cod.cliente - nr.CMR.
025300980401     C     D87LNP        IFEQ      0
025400980401     C                   Z-ADD     D87CCM        KKEY1
025500980401     C                   ELSE
025600980401     C                   Z-ADD     D87LNP        KKEY1
025700980401     C                   END
025800961010     C                   Z-ADD     D87NFE        KNFE
025900961010     C     KSTA          CHAIN     EDSTAWRK                           30
026000110330      *   Se fine File
026100110330     C                   If        *IN30 = *on
026200110330     C                   MOVEL     'S'           WFINE             1
026300110330     C                   EndIF
026400110330      *
026500961010     C                   ENDSR
026600960528     C*----------------------------------------------------------------
026700960528     C*? REDFIL - LETTURA PROSSIMO RECORD
026800960528     C*----------------------------------------------------------------
026900960528     C     REDFIL        BEGSR
027000960528     C*
027100961010     C* Deleto vecchio record
027200961010     C                   DELETE    EDSTA000
027300961010     C* Lettura file EDSTA
027400961010     C     KSTA          READE     EDSTAWRK                               30
027500110330      *   Se fine File
027600110330     C                   If        *IN30 = *on
027700110330     C                   MOVEL     'S'           WFINE             1
027800110330     C                   EndIF
027900960528     C*
028000960528     C                   ENDSR
028100961010     C*----------------------------------------------------------------
028200110414     C*? chiusura Messaggio ULTIMI Segmenti da riportare
028300110414     C*----------------------------------------------------------------
028400110414     C     End_MSG       BEGSR
028500110414      *
028600110414      *   e deve essere tradotto NON CON INTESA....
028700110414     c                   If        §PZPGMTRSS = 'STA94A'
028800110414     C                               and  Numero_Righe > 1
028900120831     c                               or
029000120831     c                             §PZPGMTRSS = 'GELSTA'
029100120831     C                               and  Numero_Righe > 0
029200110414      *
029300110414      * Se c'è stato qualche evento da inviare che quindi ha scritto
029400110414      *   chiude il messaggio altrimenti esegue un ROLLBACK
029500110414     c                   if        Righe_Dett = 'S'
029600120831      *
029700110414      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
029800110414      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
029900110414     c                   move      §MSNUM        fine04            4
030000110414     c                   eval      STATPR = 'FINE'
030100110414     c                   eval      STADAT = fine04
030200120831      *
030300120831     c                   exsr      write_OUTPUT
030400110414     c                   eval      Righe_Dett = 'N'
030500110414     c                   else
030600120831      *
030700120831      * deve uscire senza avere fatto nulla
030800110414     c                   eval      STATPR = 'RLBK'
030900120831     c                   exsr      write_OUTPUT
031000110414      *
031100110414     c                   end
031200110414     c                   end
031300110414     C*
031400110414     C                   ENDSR
031500110414     C*----------------------------------------------------------------
031600961010     C*? SAVDAT - SALVATAGGIO DATI PER SCRITTURA BOLLA
031700961010     C*----------------------------------------------------------------
031800961010     C     SAVDAT        BEGSR
031900961010     C*
032000961010     C*  Mi imposto i dati spedizione dalla prima bolla letta
032100961010     C*  per poi eseguire scrittura dettaglio a rottura bolla
032200961010     C*  codice errore
032300961010     C                   MOVEL     SPEERR        SAVSPE
032400040305     C                   MOVEL     CNI_STA       CNI_WSV
032500961010     C                   MOVEL     STACNI        WSVCNI
032600980429     C                   MOVEL     STADTR        WSVDTR
032700980429     C                   MOVEL     STAHMS        WSVHMS
032800961010     C                   MOVEA     *BLANKS       SGN
032900961010     C                   MOVEL     *BLANKS       WDAT
033000961010     C                   Z-ADD     0             WNRCOL            5 0
033100961010     C                   Z-ADD     0             Y                 3 0
033200961010     C*
033300961010     C                   ENDSR
033400961010     C*----------------------------------------------------------------
033500961010     C*? GESBOL - Carico dati sgn
033600961010     C*----------------------------------------------------------------
033700961010     C     GESBOL        BEGSR
033800961010     C*
033900961010     C                   ADD       1             WNRCOL
034000961010     C*  Eseguo caricamento det.colli se devo inviarli
034100961010     C     §PUSGD        IFEQ      'S'                                          sgn sempre
034200961010     C     §PUSGD        OREQ      'N'                                          sgn negat.
034300980402     C     STAFLE        ANDNE     *BLANKS                                      errore
034400980330     C* Controllo se devo tornare segnacollo euroexpress
034500980330     C     §PUBS1        IFEQ      'E'
034600980330     C                   EXSR      CARSGE
034700980330     C                   ELSE
034800961010     C* Calcolo nr.segnacollo da passare al cliente
034900961015     C                   EXSR      CARSGN
035000961010     C                   ADD       1             Y
035100961010     C                   MOVEL     WSGN          SGN(Y)
035200961010     C                   END
035300980330     C                   END
035400961010     C*
035500961010     C                   ENDSR
035600960528     C*----------------------------------------------------------------
035700960528     C*? WRST00 - Scrittura record partner
035800960528     C*----------------------------------------------------------------
035900960528     C     WRST00        BEGSR
036000960528     C*
036100960528     C*  Pulisco dati DS
036200960528     C                   CLEAR                   EDST00
036300960528     C*  Mittente: Bartolini  (ITBAR + AS..)
036400960528     C                   MOVEL     'ITBAR'       WMITT             8
036500961002     C                   MOVE      WAS400        WMITT             8
036600960530     C                   MOVEL     WMITT         TA0004
036700960729     C                   MOVEL     'ZZ'          TA0007
036800961010     C                   MOVEL     D87CPT        TA0010
036900961029     C                   MOVE      '    '        TA0010
037000960827     C*  Utilizzo qualificatore x destinatario msg.
037100960827     C                   MOVEL     §PTMSQ        TAA007
037200960528     C*  Data/Ora invio messagio: adesso
037300980109     C                   MOVEL     WOGGI         TA0017
037400960528     C                   MOVEL     WHHMM         TA0019
037500960528     C*  Codice documento - nome documento
037600961002     C                   MOVEL     '781'         TA1001
037700961002     C                   MOVEL     D87NFE        TA1004                         Nr.F.entrata
037800960528     C                   MOVEL     '9  '         TA1225
037900961010     C*  Data foglio entrata
038000961010     C                   MOVEL     '137'         TA2005
038100961010     C                   MOVEL     D87DFE        TA2380                         Data F.E.
038200960528     C                   MOVEL     '102'         TA2379
038300100512      *
038400100512      *    Se si vuole la data con l'orario
038500100512     c                   if        §pudate203 ='S'
038600100512     C                   MOVE      Whhmm         TA2380
038700100512     C                   MOVEL(p)  '203'         TA2379
038800100512     c                   end
038900100512      *
039000960528     C*  Scivo record IFTSTA
039100960705     C                   CLEAR                   WDAT
039200960705     C                   MOVEL     EDST00        WDAT
039300960726     C                   Z-ADD     §MSNUM        STAPRG
039400960726     C                   MOVEL     'ST00'        STATPR
039500960705     C                   MOVEL     WDAT          STADAT
039600110330      *
039700120831      *  Testata messaggio  UNB-BGM-DTM
039800120831     c                   exsr      write_OUTPUT
039900110330      *
040000960528     C                   ENDSR
040100960708     C*----------------------------------------------------------------
040200960708     C*? WRST05 - Scrittura record partner
040300960708     C*----------------------------------------------------------------
040400960708     C     WRST05        BEGSR
040500960708     C*
040600960708     C                   CLEAR                   EDST05
040700960708     C*  Ship from:
040800960712     C                   MOVEL     *BLANKS       WDAT
040900960708     C                   MOVEL     'SF '         TE3035
041000980421     C                   MOVEL     §PTPLT        TE3036
041100960708     C*  Scivo record IFCSUM
041200960708     C                   CLEAR                   WDAT
041300960708     C                   MOVEL     EDST05        WDAT
041400960726     C                   Z-ADD     §MSNUM        STAPRG
041500960712     C                   MOVEL     'ST05'        STATPR
041600960708     C                   MOVEL     WDAT          STADAT
041700110330      *
041800120831      *  Testata messaggio  NAD-SF
041900120831     c                   exsr      write_OUTPUT
042000960708     C*  Ship to:
042100960712     C                   MOVEL     *BLANKS       WDAT
042200960708     C                   CLEAR                   EDST05
042300960708     C                   MOVEL     'ST '         TE3035
042400980421     C                   MOVEL     COST1         TE3036
042500110330      *
042600110330      *  x CALBERSON --> GEODIS
042700110330      *      imposta la Platform che ci hanno chiesto di utilizzare
042800110330      *      x identificare diversamente dai dati di DUNLOP altrimenti
042900110330      *      non riescono a dividerli nel loro sistema.
043000110330      *  da impostare il §PTLOC ??????
043100110330     c                   if        §PTKSC = 493298
043200110330     C                   clear                   TE3036
043300110330     C                   MOVEL     CALB_Plat     TE3036
043400110330     c                   end
043500110330      *
043600960708     C*  Scivo record IFCSUM
043700960708     C                   CLEAR                   WDAT
043800960708     C                   MOVEL     EDST05        WDAT
043900960726     C                   Z-ADD     §MSNUM        STAPRG
044000960712     C                   MOVEL     'ST05'        STATPR
044100960708     C                   MOVEL     WDAT          STADAT
044200110330      *
044300110330      *  Testata messaggio  NAD-ST
044400120831     c                   exsr      write_OUTPUT
044500960708     C*
044600960708     C                   ENDSR
044700960528     C*----------------------------------------------------------------
044800960528     C*? WRST10 - Scrittura record partner
044900960528     C*----------------------------------------------------------------
045000960528     C     WRST10        BEGSR
045100960528     C*
045200960528     C*  Pulisco dati DS
045300110330     C                   MOVEL     STACMR        WSVCMR
045400980402     C     §PTCM1        IFEQ      'S'
045500980402     C     §PTNF1        OREQ      'S'
045600960712     C                   MOVEL     *BLANKS       WDAT
045700960528     C                   CLEAR                   EDST10
045800960726     C*  Se devo ritornare imposto AFB + nr.messaggio
045900961010     C                   MOVEL     STATPD        TB1153
046000961010     C                   MOVEL     STACMR        TB1154
046100961010     C                   MOVEL     STACMR        WSVCMR
046200960726     C*  Scivo record IFCSUM
046300960726     C                   CLEAR                   WDAT
046400960726     C                   MOVEL     EDST10        WDAT
046500960726     C                   Z-ADD     §MSNUM        STAPRG
046600960726     C                   MOVEL     'ST10'        STATPR
046700960726     C                   MOVEL     WDAT          STADAT
046800110330      *
046900120831      *  Testata messaggio  RFF-LOC
047000120831     c                   exsr      write_OUTPUT
047100110330      *
047200980402     C                   END
047300960528     C*
047400960528     C                   ENDSR
047500960528     C*----------------------------------------------------------------
047600960528     C*? WRST15 - Scrittura record partner
047700960528     C*----------------------------------------------------------------
047800960528     C     WRST15        BEGSR
047900960528     C*
048000960528     C*  Pulisco dati DS
048100960712     C                   MOVEL     *BLANKS       WDAT
048200960528     C                   CLEAR                   EDST15
048300960528     C*
048400960528     C                   ENDSR
048500960528     C*----------------------------------------------------------------
048600960528     C*? WRSD00 - Scrittura record partner - dettaglio
048700960528     C*----------------------------------------------------------------
048800960528     C     WRSD00        BEGSR
048900960528     C*
049000960528     C*  Pulisco dati DS
049100960712     C                   MOVEL     *BLANKS       WDAT
049200960528     C                   CLEAR                   EDSD00
049300120705      * Default
049400120705      *  se non richiesta l'inversione del dato CNI nell'AGE
049500120705     c                   if        §ptTRD <> 'S'
049600120705      *
049700960528     C*  Imposto numero spedizione cliente
049800040305     C                   MOVEL     CNI_WSV       DA1004
049900120705      *
050000120705      *  inverte con l'AGE
050100120705     c                   elseIF    §ptTRD  = 'S'
050200120705     C                   MOVEL(p)  WSVCNI        DA1004
050300120705     c                   end
050400120705      *
050500961010      * se non è dischetto lo segnalo
050600961015     C   01
050700961015     CANN01WSVCMR        IFEQ      *BLANKS
050800960821     C                   MOVEL     'S'           WSD15             1
050900961015     C                   ENDIF
051000961010     C*  Incremento proggr. a rottura nr.spedizione
051100961015     C     WSVSPE        IFNE      WEXSPE
051200961015     C                   MOVEL     WSVSPE        WEXSPE           16
051300961010     C                   END
051400060406      ** Deve sempre incrementare
051500060406     C                   ADD       1             WPROG
051600961010     C                   MOVEL     WPROG         DA1490
051700961010     C                   MOVEL     '9999'        DA1312
051800961010     C*  Scivo record IFTSTA
051900961010     C                   CLEAR                   WDAT
052000961010     C                   MOVEL     EDSD00        WDAT
052100961010     C                   Z-ADD     §MSNUM        STAPRG
052200961010     C                   MOVEL     'SD00'        STATPR
052300961010     C                   MOVEL     WDAT          STADAT
052400110330      *
052500120831      *  Testata messaggio  CNI-
052600120831     c                   exsr      write_OUTPUT
052700960528     C*
052800960528     C                   ENDSR
052900960528     C*----------------------------------------------------------------
053000960528     C*? WRSD05 - Scrittura record partner - dettaglio
053100960528     C*----------------------------------------------------------------
053200960528     C     WRSD05        BEGSR
053300960528     C*
053400960528     C*  Pulisco dati DS
053500960712     C                   MOVEL     *BLANKS       WDAT
053600960528     C                   CLEAR                   EDSD05
053700961010     C                   MOVEL     'AGE'         DB1153
053800120705      * Default
053900120705      *  se non richiesta l'inversione del dato CNI nell'AGE
054000120705     c                   if        §ptTRD <> 'S'
054100961010     C                   MOVEL     WSVCNI        DB1154
054200120705      *  inverte con il CNI
054300120705     c                   elseIF    §ptTRD  = 'S'
054400120705     C                   MOVEL     CNI_WSV       DB1154
054500120705     c                   end
054600120705      *
054700961015     C                   MOVEL     WSVSTS        DB9011
054800961015     C                   MOVEL     WSVERR        DB9013
054900960528     C                   MOVEL     '7  '         DB2005
055000961010     C*  Controllo se ho anche l'ora o solo la data
055100980429     C     WSVHMS        IFEQ      *ZEROS
055200980429     C                   MOVEL     WSVDTR        DB2380
055300960528     C                   MOVEL     '102'         DB2379
055400100512      *
055500100512      *    Se si vuole la data con l'orario
055600100512     c                   if        §pudate203 ='S'
055700100512     C                   MOVE      '0001'        DB2380
055800100512     C                   MOVEL(p)  '203'         DB2379
055900100512     c                   end
056000100512      *
056100961010     C                   ELSE
056200980429     C                   MOVEL     WSVDTR        WDATHH           12
056300980429     C                   MOVE      WSVHMS        WDATHH
056400961010     C                   MOVEL     WDATHH        DB2380
056500961010     C                   MOVEL     '203'         DB2379
056600961010     C                   END
056700960917     C*  Scivo record IFTSTA
056800960917     C                   CLEAR                   WDAT
056900961010     C                   MOVEL     EDSD05        WDAT
057000960917     C                   Z-ADD     §MSNUM        STAPRG
057100961010     C                   MOVEL     'SD05'        STATPR
057200960917     C                   MOVEL     WDAT          STADAT
057300110330      *
057400120831      *  Testata messaggio  STS-RFF-DTM
057500120831     c                   exsr      write_OUTPUT
057600110330      *
057700110330     C                   MOVEL     'S'           WRTEVE            1
057800110330     c                   eval      Righe_Dett = 'S'
057900110330      *
058000960528     C     FIND05        ENDSR
058100960528     C*-------------------------------------------------------------*
058200960528     C*? WRSD10 - Scrittura record partner - dettaglio
058300960528     C*----------------------------------------------------------------
058400960528     C     WRSD10        BEGSR
058500960528     C*
058600960528     C*  Pulisco dati DS
058700961010     C                   MOVEL     *BLANKS       WDAT
058800960528     C                   CLEAR                   EDSD10
058900960528     C*
059000960528     C                   ENDSR
059100960528     C*-------------------------------------------------------------*
059200960528     C*? WRSD15 - Scrittura record partner - dettaglio
059300960528     C*----------------------------------------------------------------
059400960528     C     WRSD15        BEGSR
059500960528     C*
059600960821     C*  SPEDIZ. NON RICEVUTA CON E.D.I. MA BOLLETTATA AMANO
059700960821     C     WSD15         IFEQ      'S'
059800960821     C                   MOVEL     *BLANKS       WDAT
059900960821     C                   CLEAR                   EDSD15
060000960821     C                   MOVEL     'AAI'         DD4451
060100960821     C                   MOVEL     FTX(1)        DD4440
060200960821     C                   MOVEL     EDSD15        WDAT
060300960821     C                   Z-ADD     §MSNUM        STAPRG
060400960821     C                   MOVEL     'SD15'        STATPR
060500960821     C                   MOVEL     WDAT          STADAT
060600110330      *
060700120831      *  Testata messaggio  FTX-
060800120831     c                   exsr      write_OUTPUT
060900110330      *
061000110330     c                   eval      Righe_Dett = 'S'
061100960821     C                   CLEAR                   WSD15
061200960821     C                   END
061300961010     C*
061400960528     C                   ENDSR
061500960528     C*-------------------------------------------------------------*
061600960528     C*? WRSD20 - Scrittura record partner - dettaglio
061700960528     C*----------------------------------------------------------------
061800960528     C     WRSD20        BEGSR
061900960528     C*
062000960528     C*  Pulisco dati DS
062100960528     C                   CLEAR                   EDSD20
062200960528     C                   MOVEL     '99999'       DE1496
062300961010     C*  carico dettaglio segnacolli
062400960912     C                   EXSR      CARCOL
062500960528     C*
062600960528     C                   ENDSR
062700960912     C*----------------------------------------------------------------
062800960912     C*? CARCOL - Routine caricamento colli da dettag.
062900960912     C*----------------------------------------------------------------
063000960912     C     CARCOL        BEGSR
063100960912     C*
063200961010     C*  Scrivo D20 da SGN
063300961029     C     Y             IFNE      0
063400961010     C                   MOVE      Y             DE7224
063500961015     C                   MOVEL     'PK'          DE7065
063600960912     C                   Z-ADD     0             WW                3 0
063700040218     C                   Z-ADD     0             WE                3 0
063800040217      *  imposta quante volte deve eseguire il ciclo per quanti segnacolli
063900040217      *  imposta per ogni record.
064000050428     c                   if        §Puel2 = 0
064100050428     c                   eval      §Puel2 = 1
064200050428     c                   end
064300050428      *
064400040217     c     y             div       §Puel2        volte
064500040217     c                   mvr                     resto             3 0
064600040217     c                   if        resto > 0
064700040217     c                   add       1             volte
064800040217     c                   end
064900040217      *
065000040217     C                   DO        volte         vl                3 0
065100040218      * Impacchetta sulla schiera di 20 elementi quanti elementi servono
065200040218     C                   DO        §PUEL2        WW
065300040218     c                   add       1             WE
065400040218     C                   MOVEL     SGN(WE)       D20(WW)
065500040218     c                   ENDDO
065600040218      *
065700980424     C     D20(1)        IFNE      *BLANKS
065800960913     C                   MOVEL     '24'          DE4233
065900980424     C                   ELSE
066000980424     C                   MOVEL     *BLANKS       DE4233
066100980424     C                   MOVEA     *BLANKS       D20
066200980424     C                   END
066300040218      *
066400960912     C                   EXSR      WRT20
066500040218      *
066600960912     C*  Se ci sono ancora dei colli clear tracciato
066700960912     C*  e scrittura nuovo record
066800960912     C                   MOVEL     *BLANKS       EDSD20
066900960912     C                   END
067000961029     C*
067100961029     C                   ELSE
067200961029     C                   CLEAR                   EDSD20
067300961104     C                   MOVEL     '99999'       DE1496
067400961029     C                   MOVE      WNRCOL        DE7224
067500961029     C                   MOVEL     'PK'          DE7065
067600961029     C                   EXSR      WRT20
067700961029     C                   END
067800960912     C*
067900960912     C                   ENDSR
068000960912     C*----------------------------------------------------------------
068100960912     C*? WRT20 - scrivo record d20
068200960912     C*----------------------------------------------------------------
068300960912     C     WRT20         BEGSR
068400960912     C*
068500960912     C*  Scrivo record SD20
068600051202     C*    ma solo se contiene delle informazioni altrimenti salto
068700960912     C                   MOVEL     EDSD20        WDAT
068800051202     C*
068900051202     c                   if        %subst(WDAT:10:1900) <> *blanks
069000960912     C                   MOVEL     'SD20'        STATPR
069100960912     C                   Z-ADD     §MSNUM        STAPRG
069200960912     C                   MOVEL     WDAT          STADAT
069300110330      *
069400120831      *  Testata messaggio  GID-PCI
069500120831     c                   exsr      write_OUTPUT
069600110330     c                   eval      Righe_Dett = 'S'
069700051202     c                   end
069800960912     C*
069900960912     C                   ENDSR
070000960730     C*----------------------------------------------------------------
070100960912     C*? CARSGN - caricamneto segnacollo
070200960730     C*----------------------------------------------------------------
070300960912     C     CARSGN        BEGSR
070400960730     C*
070500960730     C*  Se ha segnacollato il cliente carico in schiera i colli cons.
070600960912     C                   MOVEA     *BLANKS       C35
070700960912     C                   MOVEL     *BLANKS       WSGN             35
070800960912     C*  Compongo lnp se la devo passare
070900960912     C     §PULP2        IFNE      0
071000960912     C                   Z-ADD     §PULP2        WA                3 0
071100961029     C                   MOVEL     STAFLS        WLNP              3
071200960912     C                   MOVEA     WLNP          C35(WA)
071300960912     C                   END
071400960912     C*  Compongo lna se la devo passare
071500960912     C     §PULA2        IFNE      0
071600960912     C                   Z-ADD     §PULA2        WA                3 0
071700961010     C                   MOVEL     STALNA        WLNA              3
071800960912     C                   MOVEA     WLNA          C35(WA)
071900960912     C                   END
072000960912     C*  Compongo nrs se lo devo passare
072100960912     C     §PUNR2        IFNE      0
072200960912     C                   Z-ADD     §PUNR2        WA                3 0
072300961010     C                   MOVEL     STANRS        WNRS              2
072400960912     C                   MOVEA     WNRS          C35(WA)
072500960912     C                   END
072600960912     C*  Compongo nsc se la devo passare
072700960912     C     §PUSG2        IFNE      0
072800960912     C                   Z-ADD     §PUSG2        WA                3 0
072900961010     C                   MOVEL     STANSC        WNSC              7
073000960912     C                   MOVEA     WNSC          C35(WA)
073100960912     C                   END
073200960912     C*  Compongo znc se la devo passare
073300960912     C     §PUZN2        IFNE      0
073400960912     C                   Z-ADD     §PUZN2        WA                3 0
073500970626     C                   MOVE      STAZNC        WZNC              2
073600960912     C                   MOVEA     WZNC          C35(WA)
073700960912     C                   END
073800960912     C*  metto C35 in WSGN
073900960912     C                   MOVEA     C35           WSGN
074000960730     C*
074100960730     C                   ENDSR
074200980330     C*----------------------------------------------------------------
074300980330     C*? CARSGE - caricamneto segnacollo Euroexpress
074400980330     C*----------------------------------------------------------------
074500980330     C     CARSGE        BEGSR
074600980330     C*
074700980401     C* Controllo se ho aggiunto eccedenze euroexpress
074800980401     C     STANSC        IFEQ      *ZEROS
074900980401     C                   MOVEL     *BLANKS       WSVCNI
075000980402     C                   MOVEL     '0'           WSVCNI
075100980401     C                   Z-ADD     1             Y
075200980401     C                   MOVEL     STACNI        SGN(Y)
075300980401     C* Abblenco SAVSPE
075400980401     C                   MOVEL     *BLANKS       SAVSPE
075500980402     C                   MOVEL     *LOVAL        WEXSPE
075600980401     C                   ELSE
075700980401     C* Aggancio segnacollo eroexpress
075800980401     C                   Z-ADD     STAFLS        KFLS
075900980401     C                   Z-ADD     STALNA        KLNA
076000980401     C                   Z-ADD     STANRS        KNRS
076100980401     C                   Z-ADD     STANSC        KNSC
076200980330     C                   MOVEL     'E'           KTRC
076300051017     C                   CLEAR                   FIARS000
076400051017     C     KARS          CHAIN     FIARS01L                           33
076500061031      *
076600061031      *  Se si tratta di un cliente che riceve i dati non IFCSUM ma da VAB
076700061031      *  i tipi records su ARS sono classificati come "C" diskC
076800061031     C   33              MOVEL     'C'           KTRC
076900061031     C   33KARS          CHAIN     FIARS01L                           33
077000980330     C                   ADD       1             Y
077100051017     C  N33              MOVEL     ARSNOT        SGN(Y)
077200980401     C                   END
077300980330     C*
077400980330     C                   ENDSR
077500960927     C*----------------------------------------------------------------
077600960927     C*? CARNUM - CARICAMENTO NUMERATORE
077700960927     C*----------------------------------------------------------------
077800960927     C     CARNUM        BEGSR
077900960927     C*
078000960927     C                   Z-ADD     0             WPROG             4 0
078100960927     C                   Z-ADD     0             X                 3 0
078200960927     C                   Z-ADD     1             §MSNUM
078300961010     C                   MOVEL     'MS'          KCOD
078400961010     C                   MOVEL     *BLANKS       KKEY
078500961010     C                   MOVEL     '781'         KKEY
078600961010     C     KTAB          CHAIN     EDTAB01L                           30
078700960927     C     *IN30         IFEQ      '0'
078800960927     C     TABFLG        ANDEQ     *BLANKS
078900960927     C                   MOVEL     TABUNI        DSMS
079000960927     C                   ADD       1             §MSNUM
079100061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
079200061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
079300061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
079400061219     c                   move      §MSNUM        campo_4n          4 0
079500061219     c                   if        campo_4n = 0
079600061219     c                   add       1             §MSNUM
079700061219     c                   end
079800960927     C                   Z-ADD     WOGGI         §MSDAT
079900960927     C                   MOVEL     DSMS          TABUNI
080000960927     C                   UPDATE    EDTAB000
080100960927     C                   END
080200960927     C*
080300960927     C                   ENDSR
080400960528     C*----------------------------------------------------------------
080500960528     C*? *INZSR - OPERAZIONI INIZIALI
080600960528     C*----------------------------------------------------------------
080700960528     C     *INZSR        BEGSR
080800960530     C*
080900960530     C     *ENTRY        PLIST
081000960530     C                   PARM                    KPJBA
081100110630     C                   PARM                    Righe_Dett
081200110630     C                   PARM                    Esito             1            ritorno errore
081300110630      *
081400110630      * reset campo compattazione x Download file
081500110630     C                   eval      esito = '0'
081600110630     c                   eval      Compatta_80 = 'S'
081700110704     c                   eval      Righe_Dett = 'N'
081800110601      *
081900961010     C                   MOVEL     KPJBU         TRTC87
082000110601      *
082100960528     C* Definisco chiavi di accesso
082200961010     C     KTAB          KLIST
082300961010     C                   KFLD                    KCOD
082400961010     C                   KFLD                    KKEY
082500120831      *
082600961010     C     KSTA          KLIST
082700980401     C                   KFLD                    KKEY1
082800961010     C                   KFLD                    KNFE
082900120831      *
083000051017     C     KARS          KLIST
083100980330     C                   KFLD                    KFLS
083200980330     C                   KFLD                    KLNA
083300980330     C                   KFLD                    KNRS
083400980330     C                   KFLD                    KNSC
083500980330     C                   KFLD                    KTRC
083600120831      *
083700120831      * DEVE ESSERE PILOTATO DALLA TAB.3K
083800120831     C     KTAB2         KLIST
083900120831     C                   KFLD                    TBLKUT
084000120831     C                   KFLD                    TBLCOD
084100120831     C                   KFLD                    TBLKEY
084200120831      *
084300960528     C* Definizione campi di work
084400051017     C     *LIKE         DEFINE    ARSFLS        KFLS
084500051017     C     *LIKE         DEFINE    arSLNA        KLNA
084600051017     C     *LIKE         DEFINE    arSNRS        KNRS
084700051017     C     *LIKE         DEFINE    arSNSC        KNSC
084800051017     C     *LIKE         DEFINE    arSTRC        KTRC
084900980330     C     *LIKE         DEFINE    TABCOD        KCOD
085000961010     C     *LIKE         DEFINE    TABKEY        KKEY
085100980401     C     *LIKE         DEFINE    STAKEY        KKEY1
085200961010     C     *LIKE         DEFINE    STANFE        KNFE
085300961010     C     *LIKE         DEFINE    STACMR        WSVCMR
085400961010     C     *LIKE         DEFINE    STACNI        WSVCNI
085500980429     C     *LIKE         DEFINE    STADTR        WSVDTR
085600980429     C     *LIKE         DEFINE    STAHMS        WSVHMS
085700040217     C     *LIKE         DEFINE    y             volte
085800960528     C*
085900960528     C* Recupero codice AS
086000960528     C                   Z-ADD     1             CODUT             1 0
086100960705     C                   CALL      'X§PARUT'
086200960528     C                   PARM                    UTEDSE
086300960528     C                   MOVEL     REC80         CNCR80
086400980622     C                   MOVEL     '005'         WAS400            3
086500960528     C* Recupero data e ora x messaggio
086600960528     C                   TIME                    WHHDAT           14 0
086700960708     C                   MOVE      WHHDAT        G02DAT
086800960528     C                   MOVE      WHHDAT        DATA              6 0
086900960528     C                   MOVEL     *BLANK        G02ERR
087000960528     C                   CALL      'XSRDA8'
087100960528     C                   PARM                    WLBDA8
087200960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
087300960708     C                   MOVEL     WHHDAT        WHHMM             4 0
087400960528     C                   MOVE      WOGGI         WOGGI6            6 0
087500960528     C                   MOVE      WHHDAT        WANNO             2 0
087600960528     C                   MOVEL     WANNO         WOGGI6
087700960528     C* Caricamento Tabella Partner esteri
087800960528     C                   Z-ADD     0             X                 3 0
087900961015     C                   MOVEL     'PT'          KCOD
088000961015     C                   MOVEL     D87CPT        KKEY
088100120831     c                   clear                   dsPT
088200961010     C     KTAB          CHAIN     EDTAB01L                           30
088300961010     C     *IN30         IFEQ      '0'
088400961010     C     TABFLG        ANDEQ     *BLANKS
088500961010     C                   MOVEL     TABUNI        DSPT
088600960528     C                   END
088700961015     C                   MOVEL     'PU'          KCOD
088800120831     c                   clear                   dsPU
088900961010     C     KTAB          CHAIN     EDTAB01L                           30
089000961010     C     *IN30         IFEQ      '0'
089100961010     C     TABFLG        ANDEQ     ' '
089200961010     C                   MOVEL     TABUNI        DSPU
089300960912     C                   END
089400110330      *   Con quale traduttore deve essere scaricato e scritto il File ????
089500110330     C                   MOVEL     'PZ'          KCOD
089600120831     c                   clear                   dsPZ
089700110330     C     KTAB          CHAIN     EDTAB01L
089800110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
089900110330     C                   MOVEL     TABUNI        DSPZ
090000110330     C                   END
090100120831      *
090200120831     c                   eval      CLIENTE =  '0' + %editc(§PTKSC:'X')
090300120831     C                   Z-ADD     1             TBLKUT
090400120831     C                   MOVEL     '3K'          TBLCOD
090500120831     C                   moveL(P)  §PTKSC        TBLKEY
090600120831     C     KTAB2         chain     tabel00f
090700120831     c                   if        %Found(tabel00f)
090800120831     c                   eval      ds3K = tblUNI
090900120831     c                   eval      CLIENTE =  '0' + %editc(§3Kcks:'X')
091000120831     C                   END
091100960528     C* finta read x IFTSTA
091200960528     C   01
091300110407     CANN01              READ      EDSTA00F                               01
091400960528     C*
091500960528     C                   ENDSR
091600120831      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
091700120831      *  Scrive l'OUTPUT da inviare
091800120831      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
091900120831     C     write_OUTPUT  BEGSR
092000120831      *
092100120831      * Scrittura file per 20/210 Appuntamento da ATR
092200120831     c                   if        STATPR <> 'FINE' and
092300120831     c                             STATPR <> 'RLBK'
092400120831     C                   WRITE     IFTSTA00
092500120831     c                   end
092600120831      *
092700120831     c                   If        §PZPGMTRSS = 'STA94A'
092800120831     c                   exsr      Exit_segmenti
092900120831     c                   elseIf    §PZPGMTRSS = 'GELSTA'
093000120831     c                   exsr      Exit_FileGEL
093100120831     C                   EnD
093200120831      *
093300120831     C                   ENDSR
093400110330      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
093500110330      *  Scrittura dei segmenti IFTSTA D94A
093600110330      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
093700110330     C     Exit_Segmenti BEGSR
093800120413      *
093900110330     c                   eval      Tipo_DS = STATPR
094000110330     c                   eval      Suffisso   = §PUSIG
094100110601     c                   eval      Compatta_80 = §pucompat
094200110330     c                   clear                   DsGenerica
094300110330     c                   eval      DsGenerica = STADAT
094400110330     c                   call      'TRTCT81R1'
094500110330     c                   PARM                    CLIENTE           8
094600110330     c                   PARM                    Suffisso          2
094700110330     c                   PARM                    Tipo_DS           4
094800110330     c                   PARM                    DsGenerica     1024
094900110330     C                   PARM                    Numero_Righe      5 0
095000110601     c                   PARM                    Compatta_80       1
095100110617     C                   PARM                    Progressivo      10
095200110621     c                   PARM                    esito             1
095300110630     C
095400110630     C                   if        esito = 'E'
095500110630     c                   exsr      Invio_Mail
095600110630     c                   end
095700110330      *
095800110330     C                   ENDSR
095900120831     c*==================================================================*
096000120831      * Manda un File a tracciato NETEXPRESS EUROPE
096100120831     c*==================================================================*
096200120831     c     Exit_FileGEL  begsr
096300120831      *
096400120831     c                   clear                   DsGenerica
096500120831     c                   eval      Tipo_DS = STATPR
096600120831     c                   eval      DsGenerica = STADAT
096700120905     c                   call      'TRTCT81GR'
096800120831     c                   PARM                    CLIENTE
096900120831     c                   PARM                    Tipo_DS           4
097000120831     c                   PARM                    DsGenerica     1024
097100120831     C                   PARM                    Numero_Righe      5 0
097200120831     C                   PARM                    Progressivo      10
097300120831     c                   PARM                    esito             1
097400120831     C
097500120831     C                   if        esito = 'E'
097600120831     c                   exsr      Invio_Mail
097700120831     c                   end
097800120831      *
097900120831     C                   ENDSR
098000110630     c*==================================================================*
098100110630      * Manda un Msg x E-mail
098200110630     c*==================================================================*
098300110630     c     Invio_Mail    begsr
098400110630      *
098500120831     c                   clear                   CLIENTEksc        8
098600120831     c                   eval      CLIENTEksc =  '0' + %editc(§PTKSC:'X')
098700110630???? C                   EVAL      Oggetto ='Problemi DOWNLD ATR STATI EDI !!'
098800110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione ATR EDI-
098900110630     C                              STATI NON tradotto x invio su DOWNLD. -
099000110630     C                             Controllare il Download del Cliente. << '+
099100120831     c                             CLIENTEksc + ' >>'
099200110630      *
099300110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
099400110630     C* Inizializzo variabili
099500110630     C                   movel     *blanks       wrkEml          253
099600110630     C                   movel     *blanks       wrkMsg         5000
099700110630     C                   movel     *blanks       wrkOgg           44
099800110630      *
099900110630     C* Valorizzo i campi della e-m@ail - indirizzo
100000110630     C                   eval      wrkEml = CED_BART
100100110630     C                   eval      wrkOgg = Oggetto
100200110630     C                   eval      wrkMsg = Messaggio
100300110630     C                   call(e)   'TRTCT00R2'
100400110630     C                   parm                    wrkEml
100500110630     C                   parm                    wrkOgg
100600110630     C                   parm                    wrkMsg
100700110630     C*
100800110630     C                   ENDSR
100900110630      * ?------------------------------------------------------------------ */
101000110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
101100110630      * ?------------------------------------------------------------------ */
101200110630     C     *pssr         BEGSR
101300110630     C
101400110630     C                   eval      esito = 'E'
101500110630     c                   exsr      Invio_Mail
101600110630     C
101700110630     C                   ENDSR     '*CANCL'
101800110630     C
101900110630      *  ------------------------------------------------------------------ */
102000961010** FTX
102100961010RECEIVED REFERRING TO A SHIPMENT NOT RECEIVED WITH E.D.I. MESSAGE
