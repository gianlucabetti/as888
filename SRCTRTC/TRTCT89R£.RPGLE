000100930128      ***********************************************************************
000200961002      *   RITORNO DATE ARRIVO SPEDIZIONI              ESTERO                *
000300930128      ***********************************************************************
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500960528     F*---------------------------------------------------------------------*
000600960528     F*  DATA BASE                                                          *
000700960528     F*---------------------------------------------------------------------*
000800110418     FEDSTA00F  UF   E             DISK    usropn
000900961002     F*---------
001000120831     Ftabel00f  IF   E           K DISK
001100961002     FEDTAB01L  UF   E           K DISK
001200980330     F*---------
001300040707     FFNBLP01L  IF   E           K DISK
001400960528     D*---------------------------------------------------------------------*
001500960528     D* Schiere
001600960528     D*---------------------------------------------------------------------*
001700960528     D* Schiera per scrittura dati località record ST10
001800960528     D* Schiera per scrittura testi liberi record ST15
001900960528     D* Schiera per scrittura testi liberi record SD15
002000960528     D KSC             S              7  0 DIM(100)
002100010409     D FTT             S              1    DIM(100)
002200960821     D* TESTO LIBERO
002300960821     D FTX             S             70    DIM(1) CTDATA PERRCD(1)
002400960912     D* composizione schiera 35 chr x segnacollo
002500960912     D C35             S              1    DIM(35)
002600960528     D*---------------------------------------------------------------------*
002700960528     D* DS
002800960528     D*---------------------------------------------------------------------*
002900960528     D* .. per scompozione dati da spedire a seconda del tipo record
003000960528     D EDST00        E DS                  EXTNAME(EDST00DS)
003100960708     D EDST05        E DS                  EXTNAME(EDST05DS)
003200960528     D EDST10        E DS                  EXTNAME(EDST10DS)
003300960528     D  T10                   48    103
003400960528     D                                     DIM(2)
003500960528     D EDST15        E DS                  EXTNAME(EDST15DS)
003600960528     D  T15                   13    362
003700960528     D                                     DIM(5)
003800960528     D EDSD00        E DS                  EXTNAME(EDSD00DS)
003900960528     D EDSD05        E DS                  EXTNAME(EDSD05DS)
004000960528     D EDSD10        E DS                  EXTNAME(EDSD10DS)
004100960528     D EDSD15        E DS                  EXTNAME(EDSD15DS)
004200960528     D  D15                   13    362
004300960528     D                                     DIM(5)
004400960528     D EDSD20        E DS                  EXTNAME(EDSD20DS)
004500960730     D  D20                   33    732
004600960730     D                                     DIM(20)
004700960528     D*---------------------------------------------------------------------*
004800960528     D* .. per reperimento tipo record di EDIFTSTA da scrivere
004900960528     D*                    .. 5 - 8  : tipo record
005000040218     D WDAT            DS          1950
005100960729     D  STAPRG                 1      4  0
005200960726     D  STATPR                 5      8
005300960528     D*---------------------------------------------------------------------*
005400960528     D KPJBA         E DS
005500960528     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
005600960528     D CNCR80        E DS
005700120831     D DS3K          E DS
005800110330     D DSPT          E DS                  EXTNAME(EDIDSPT)
005900110330     D DSPU          E DS                  EXTNAME(EDIDSPU)
006000110330     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
006100110330     D DSPW          E DS                  EXTNAME(EDIDSPW)
006200110330     D DSPV          E DS                  EXTNAME(EDIDSPV)
006300960726     D DSMS          E DS                  EXTNAME(EDIDSMS)
006400110330     D TRTC87        E DS                  EXTNAME(TRTC87DS)
006500110330     d Righe_Dett      s              1
006600961010     D*  DS x rottura livello spedizione - codice errore
006700961010     D SAVSPE          DS
006800040305     D  CNI_WSV                3     16  0
006900961010     D  WSVAAS                 1      4  0
007000961010     D  WSVLNP                 5      7  0
007100961010     D  WSVNRS                 8      9  0
007200961010     D  WSVNSP                10     16  0
007300961015     D  WSVSPE                 1     16
007400961010     D  WSVSTS                17     19
007500961010     D  WSVERR                20     22
007600960528     D WLBDA8          DS
007700960528     D  G02DAT                 1      8  0
007800960528     D  G02INV                 9     16  0
007900960528     D  G02ERR                17     17
008000960528     D  G02TGI                18     22  0
008100110630      *----------------------------------------------------*
008200110630      *  Invio E-mail
008300110630     D Indirizzi       s            253
008400110630     D Oggetto         s             44
008500110630     D Messaggio       s           5000
008600110630      *
008700110630     d   DSmail        ds
008800110630     D Mail_Ind                      44
008900110630     d   IND_char                     1    dim(44)
009000110630      *
009100110630     D Lunghezza_Indirizzo...
009200110630     D                 s              5u 0
009300110630     C*---------------------------------------------------------------*
009400110630     D MsgDSP          S            256                                         Testo generico
009500110630     C*---------------------------------------------------------------*
009600110630     D SndMg01         S             10                                         Message type
009700110630     D                                     INZ('*INFO')
009800110630     D SndMg02         S             10                                         Deluvery mode
009900110630     D                                     INZ('*BREAK')
010000110630     D SndMg03         S            256                                         Message text
010100110630     D SndMg04         S             10I 0                                      Length of text
010200110630     D                                     INZ(%SIZE(SndMg03))
010300110630     D SndMg05         S             10                                         User profile
010400110630     D                                     DIM(299)
010500110630     D SndMg06         S             10I 0                                      Number of user
010600110630     D SndMg07         S             10I 0                                      Message sent indic.
010700110630     D SndMg08         S             10I 0                                      Function requested
010800110630     D SndMg10         S              1                                         Show display
010900110630     D                                     INZ('N')
011000110630     D SndMg11         S             20                                         Qualified MSGQ name
011100110630     D SndMg12         S              4                                         Name type indicator
011200110630     D                                     INZ('*USR')
011300110630      * indice di scihera
011400110630     D Jx              S              5I 0
011500110630      *
011600110630     D/COPY QSYSINC/QRPGLESRC,QUSEC
011700110630      *
011800120905     d bart_it         C                   CONST('@Brt.it;')
011900120905     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
012000960708     D*
012100960528     I*---------------------------------------------------------------------*
012200960528     I* FILE
012300960528     I*---------------------------------------------------------------------*
012400960528     IIFTSTA00
012500960528     I              SSIFTSTA                    STADAT
012600960528     C*---------------------------------------------------------------------*
012700960528     C* Ciclo principale
012800960528     C*---------------------------------------------------------------------*
012900040708     C*
013000040708     C* resetta il flag x almeno una informazione supermercato
013100040708     c                   move      'N'           flg_almenouno     1
013200040707      *
013300960528      * Posizionamento iniziale sul file  (prima lettura)
013400110418     C                   open      EDSTA00F
013500110418     C                   read      EDSTA00F
013600110418     c                   exsr      EoF_EDSTA00F
013700961010      *
013800961010      * Loop letture fino a fine file EDSTA
013900110418     C                   DOW       not %eof(EDSTA00F)
014000040707     C                   MOVEL     STADAT        WDAT
014100040708      *
014200040707     C     STATPR        CASEQ     'ST00'        WRST00
014300040707     C     STATPR        CASEQ     'ST05'        WRST05
014400040707     C     STATPR        CASEQ     'ST10'        WRST10
014500040707     C     STATPR        CASEQ     'ST15'        WRST15
014600040708     C     STATPR        CASEQ     'SD00'        WRSD00
014700040708     C     STATPR        CASEQ     'SD05'        WRSD05
014800040708     C     STATPR        CASEQ     'SD10'        WRSD10
014900040708     C     STATPR        CASEQ     'SD15'        WRSD15
015000040708     C     STATPR        CASEQ     'SD20'        WRSD20
015100040708     C                   END
015200110330      *
015300110418     C                   read      EDSTA00F
015400110418     c                   exsr      EoF_EDSTA00F
015500961015     C                   END
015600110418     C                   close     EDSTA00F
015700040708      *
015800040708      * Se non ci sono righe da elaborare elimino tutto
015900040708     c                   if        flg_almenouno = 'N'
016000110418     C                   open      EDSTA00F
016100110418     C                   read      EDSTA00F
016200110418     C                   DOW       not %eof(EDSTA00F)
016300040708     C                   delete    IFTSTA00
016400110418     C                   read      EDSTA00F
016500040708     C                   END
016600040708     C                   END
016700960528     C* Fine pgm.
016800110418     C                   close     EDSTA00F
016900121010      **
017000121010      *   se si tratta di un traduttore NON EDIFACT deve far saltare al CL la parte
017100121010      *    successiva di trasmissione
017200121010     c                   if        §PZPGMTRSS = 'GELSTA'
017300131007     c                                or
017400131007     c                             §PZPGMTRSS = 'AMZSTA'
017500121010     c                   eval      esito = 'N'
017600121010     c                   end
017700121010      *
017800960528     C                   SETON                                        LR
017900110330     C*----------------------------------------------------------------
018000110330     C*? x Fine File
018100110330     C*----------------------------------------------------------------
018200110418     C     EoF_EDSTA00F  BEGSR
018300110330      *
018400110330     C* se finito il File EoF
018500110418     C                   If          %eof(EDSTA00F)
018600110330     C                   MOVEL     'S'           WFINE             1
018700130821      *
018800130821      *  Solo se ci sono stati precedentemente dei records da scrivere
018900130821     c                   IF        flg_almenouno <> 'N'
019000110330      *
019100110330      *   e deve essere tradotto NON CON INTESA....
019200110330     c                   If        §PZPGMTRSS = 'STA94A'
019300110330     C                               and  Numero_Righe > 1
019400120831     c                               or
019500120831     c                             §PZPGMTRSS = 'GELSTA'
019600120831     C                               and  Numero_Righe > 0
019700131007     c                               or
019800131007     c                             §PZPGMTRSS = 'AMZSTA'
019900131007     C                               and  Numero_Righe > 0
020000120831      *
020100110330      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
020200110330      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
020300110819     C                   clear                   STADAT
020400110819     C                   clear                   wDAT
020500110819     c                   movel     'FINE'        STATPR
020600120831      *
020700120831     c                   exsr      write_OUTPUT
020800110330     c                   eval      Righe_Dett = 'N'
020900110413      *
021000110413     c                   else
021100110413      * deve uscire senza avere fatto nulla
021200110413     c                   eval      STATPR = 'RLBK'
021300120831     c                   exsr      write_OUTPUT
021400110330     c                   end
021500130821      *
021600130821     C                   EndIF
021700110330      *
021800110330     C                   EndIF
021900110330      *
022000110330     c                   EndSR
022100960528     C*----------------------------------------------------------------
022200040707     C*? WRST00 - Aggiorna da ATR a STA normali 781 diventa 784
022300960528     C*----------------------------------------------------------------
022400960528     C     WRST00        BEGSR
022500050902      *
022600050902     C* Caricamento NUMERATORE
022700050902     C                   EXSR      CARNUM
022800960528     C*  Pulisco dati DS
022900960528     C                   CLEAR                   EDST00
023000040707     C                   movel     STADAT        EDST00
023100960528     C*  Codice documento - nome documento
023200040707     C                   MOVEL     '784'         TA1001
023300960705     C                   MOVEL     EDST00        WDAT
023400120831      *  Testata messaggio  UNB-BGM-DTM
023500120831     c                   exsr      write_OUTPUT
023600960528     C*
023700960528     C                   ENDSR
023800960708     C*----------------------------------------------------------------
023900040707     C*? WRST05 - Nulla
024000960708     C*----------------------------------------------------------------
024100960708     C     WRST05        BEGSR
024200960708     C*
024300040708     C*  Pulisco dati DS
024400040708     C                   CLEAR                   EDST05
024500040708     C                   movel     STADAT        EDST05
024600040708     C                   MOVEL     EDST05        WDAT
024700120831      *  Testata messaggio  NAD-SF / NAD-ST
024800120831     c                   exsr      write_OUTPUT
024900040708     C*
025000960708     C                   ENDSR
025100960528     C*----------------------------------------------------------------
025200040707     C*? WRST10 - Nulla
025300960528     C*----------------------------------------------------------------
025400960528     C     WRST10        BEGSR
025500960528     C*
025600040708     C*  Pulisco dati DS
025700040708     C                   CLEAR                   EDST10
025800040708     C                   movel     STADAT        EDST10
025900040708     C                   MOVEL     EDST10        WDAT
026000120831      *  Testata messaggio  RFF-LOC
026100120831     c                   exsr      write_OUTPUT
026200040708     C*
026300960528     C                   ENDSR
026400960528     C*----------------------------------------------------------------
026500040707     C*? WRST15 - nulla
026600960528     C*----------------------------------------------------------------
026700960528     C     WRST15        BEGSR
026800960528     C*
026900040708     C*  Pulisco dati DS
027000040708     C                   CLEAR                   EDST15
027100040708     C                   movel     STADAT        EDST15
027200040708     C                   MOVEL     EDST15        WDAT
027300120831      *  Testata messaggio  FTX
027400120831     c                   exsr      write_OUTPUT
027500040708     C*
027600960528     C                   ENDSR
027700960528     C*----------------------------------------------------------------
027800040707     C*? WRSD00 - Aggancia la bolla x vedere se Supermercato
027900960528     C*----------------------------------------------------------------
028000960528     C     WRSD00        BEGSR
028100040708     C*
028200040708     C* resetta il flag x update o delete
028300040708     c                   move      'N'           flg_update        1
028400040708     C*
028500040708     C*  Pulisco dati DS
028600040708     C                   CLEAR                   EDSD00
028700040708     C                   MOVEL     STADAT        EDSD00
028800040708     C*  Imposto numero spedizione cliente
028900040708     C                   MOVEL     DA1004        CNI_WSV
029000040708     C                   MOVEL     '20'          wsvAAS
029100040708     **
029200040708     ** aggancia la bolla x controllare se supermercato
029300040708     c     kblp          chain     FNblp01l
029400040708     c                   if        %Found(FnBLP01l) and
029500040721     c                             (blpTC1 = 'S' or blpTC2 = 'S' or
029600040721     c                              blpTC1 = 'A' or blpTC2 = 'A')
029700040708     c                   move      'S'           flg_update
029800040708     c                   end
029900120831      *
030000040708     c                   if        flg_update = 'S'
030100120831     c                   move      'S'           flg_almenouno
030200040708     C                   ADD       1             WPROG
030300040708     C                   z-add     WPROG         DA1490
030400040708     C                   MOVEL     EDSD00        WDAT
030500120831      *  Testata messaggio  CNI-
030600120831     c                   exsr      write_OUTPUT
030700110330     C*
030800040708     c                   else
030900040708     C                   delete    IFTSTA00
031000040708     c                   end
031100960528     C*
031200960528     C                   ENDSR
031300960528     C*----------------------------------------------------------------
031400960528     C*? WRSD05 - Scrittura record partner - dettaglio
031500960528     C*----------------------------------------------------------------
031600960528     C     WRSD05        BEGSR
031700960528     C*
031800040708     C*  Pulisco dati DS
031900040708     C                   CLEAR                   EDSD05
032000040708     C                   MOVEL     STADAT        EDSD05
032100040708      *
032200040708      *  20+210 --> segnala se consegnata a un Supermercato
032300040708     C                   MOVEL     '20 '         DB9011
032400040708     C                   MOVEL     '210'         DB9013
032500120831      **
032600040708     c                   if        flg_update = 'S'
032700040708     C                   MOVEL     EDSD05        WDAT
032800120831      *  Testata messaggio  STS-RFF-DTM
032900120831     c                   exsr      write_OUTPUT
033000110330      *
033100110330     C                   MOVEL     'S'           WRTEVE            1
033200110330     c                   eval      Righe_Dett = 'S'
033300040708     c                   else
033400120831      *
033500040708     C                   delete    IFTSTA00
033600040708     c                   end
033700961010     C*
033800040708     C                   ENDSR
033900960528     C*-------------------------------------------------------------*
034000960528     C*? WRSD10 - Scrittura record partner - dettaglio
034100960528     C*----------------------------------------------------------------
034200960528     C     WRSD10        BEGSR
034300040708     C*
034400040708     C*  Pulisco dati DS
034500040708     C                   CLEAR                   EDSD10
034600040708     C                   MOVEL     STADAT        EDSD10
034700040708      *
034800040708     c                   if        flg_update = 'S'
034900040708     C                   MOVEL     EDSD10        WDAT
035000120831      *  Testata messaggio  NAD
035100120831     c                   exsr      write_OUTPUT
035200110330      *
035300040708     c                   else
035400040708     C                   delete    IFTSTA00
035500040708     c                   end
035600040708     C*
035700960528     C                   ENDSR
035800960528     C*-------------------------------------------------------------*
035900960528     C*? WRSD15 - Scrittura record partner - dettaglio
036000960528     C*----------------------------------------------------------------
036100960528     C     WRSD15        BEGSR
036200960528     C*
036300040708     C*  Pulisco dati DS
036400040708     C                   CLEAR                   EDSD15
036500040708     C                   MOVEL     STADAT        EDSD15
036600120831      *
036700040708     c                   if        flg_update = 'S'
036800040708     C                   MOVEL     EDSD15        WDAT
036900120831      *  Testata messaggio  FTX-
037000120831     c                   exsr      write_OUTPUT
037100110330     c                   eval      Righe_Dett = 'S'
037200120831      *
037300040708     c                   else
037400040708     C                   delete    IFTSTA00
037500040708     c                   end
037600040708     C*
037700960528     C                   ENDSR
037800960528     C*-------------------------------------------------------------*
037900960528     C*? WRSD20 - Scrittura record partner - dettaglio
038000960528     C*----------------------------------------------------------------
038100960528     C     WRSD20        BEGSR
038200960912     C*
038300040708     C*  Pulisco dati DS
038400040708     C                   CLEAR                   EDSD20
038500040708     C                   MOVEL     STADAT        EDSD20
038600120831     C*
038700040708     c                   if        flg_update = 'S'
038800040708     C                   MOVEL     EDSD20        WDAT
038900120831      *  Testata messaggio  GID-PCI
039000120831     c                   exsr      write_OUTPUT
039100110330     c                   eval      Righe_Dett = 'S'
039200110330     C*
039300040708     c                   else
039400040708     C                   delete    IFTSTA00
039500040708     c                   end
039600960912     C*
039700960912     C                   ENDSR
039800960730     C*----------------------------------------------------------------
039900960927     C*? CARNUM - CARICAMENTO NUMERATORE
040000960927     C*----------------------------------------------------------------
040100960927     C     CARNUM        BEGSR
040200960927     C*
040300960927     C                   Z-ADD     0             WPROG             4 0
040400960927     C                   Z-ADD     0             X                 3 0
040500960927     C                   Z-ADD     1             §MSNUM
040600961010     C                   MOVEL     'MS'          KCOD
040700961010     C                   MOVEL     *BLANKS       KKEY
040800040707     C                   MOVEL     '784'         KKEY
040900961010     C     KTAB          CHAIN     EDTAB01L                           30
041000960927     C     *IN30         IFEQ      '0'
041100960927     C     TABFLG        ANDEQ     *BLANKS
041200960927     C                   MOVEL     TABUNI        DSMS
041300960927     C                   ADD       1             §MSNUM
041400061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
041500061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
041600061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
041700061219     c                   move      §MSNUM        campo_4n          4 0
041800061219     c                   if        campo_4n = 0
041900061219     c                   add       1             §MSNUM
042000061219     c                   end
042100960927     C                   Z-ADD     WOGGI         §MSDAT
042200960927     C                   MOVEL     DSMS          TABUNI
042300960927     C                   UPDATE    EDTAB000
042400960927     C                   END
042500960927     C*
042600960927     C                   ENDSR
042700960528     C*----------------------------------------------------------------
042800960528     C*? *INZSR - OPERAZIONI INIZIALI
042900960528     C*----------------------------------------------------------------
043000960528     C     *INZSR        BEGSR
043100960530     C*
043200960530     C     *ENTRY        PLIST
043300960530     C                   PARM                    KPJBA
043400110630     C                   PARM                    Esito             1            ritorno errore
043500110330     C                   MOVEL     KPJBU         TRTC87
043600110601      *
043700110601      * reset campo compattazione x Download file
043800110630     C                   eval      esito = '0'
043900110601     c                   eval      Compatta_80 = 'S'
044000040708     C*
044100960528     C* Definisco chiavi di accesso
044200961010     C     KTAB          KLIST
044300961010     C                   KFLD                    KCOD
044400961010     C                   KFLD                    KKEY
044500040707     **
044600040708     C     Kblp          KLIST
044700040708     C                   KFLD                    WsvAAS
044800040708     C                   KFLD                    WsvLNP
044900040708     C                   KFLD                    WsvNRS
045000040708     C                   KFLD                    WsvNSP
045100120831      *
045200120831      * DEVE ESSERE PILOTATO DALLA TAB.3K
045300120831     C     KTAB2         KLIST
045400120831     C                   KFLD                    TBLKUT
045500120831     C                   KFLD                    TBLCOD
045600120831     C                   KFLD                    TBLKEY
045700040708     **
045800980330     C     *LIKE         DEFINE    TABCOD        KCOD
045900961010     C     *LIKE         DEFINE    TABKEY        KKEY
046000960528     C*
046100960528     C* Recupero codice AS
046200960528     C                   Z-ADD     1             CODUT             1 0
046300960705     C                   CALL      'X§PARUT'
046400960528     C                   PARM                    UTEDSE
046500960528     C                   MOVEL     REC80         CNCR80
046600980622     C                   MOVEL     '005'         WAS400            3
046700040708     C*
046800960528     C* Recupero data e ora x messaggio
046900960528     C                   TIME                    WHHDAT           14 0
047000960708     C                   MOVE      WHHDAT        G02DAT
047100960528     C                   MOVE      WHHDAT        DATA              6 0
047200960528     C                   MOVEL     *BLANK        G02ERR
047300960528     C                   CALL      'XSRDA8'
047400960528     C                   PARM                    WLBDA8
047500960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
047600960708     C                   MOVEL     WHHDAT        WHHMM             4 0
047700960528     C                   MOVE      WOGGI         WOGGI6            6 0
047800960528     C                   MOVE      WHHDAT        WANNO             2 0
047900960528     C                   MOVEL     WANNO         WOGGI6
048000040708     C*
048100960528     C* Caricamento Tabella Partner esteri
048200960528     C                   Z-ADD     0             X                 3 0
048300110330     C                   MOVEL     'PT'          KCOD
048400110330     C                   MOVEL     D87CPT        KKEY
048500120831     C                   clear                   DSPT
048600110330     C     KTAB          CHAIN     EDTAB01L                           30
048700110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
048800110330     C                   MOVEL     TABUNI        DSPT
048900110330     C                   END
049000110330     C                   MOVEL     'PU'          KCOD
049100120831     C                   clear                   DSPU
049200110330     C     KTAB          CHAIN     EDTAB01L                           30
049300110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
049400110330     C                   MOVEL     TABUNI        DSPU
049500110330     C                   END
049600110330      *   Con quale traduttore deve essere scaricato e scritto il File ????
049700110330     C                   MOVEL     'PZ'          KCOD
049800120831     C                   clear                   DSPZ
049900110330     C     KTAB          CHAIN     EDTAB01L
050000110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
050100110330     C                   MOVEL     TABUNI        DSPZ
050200110330     C                   END
050300120831      *
050400120831     c                   eval      CLIENTE =  '0' + %editc(§PTKSC:'X')
050500120831     C                   Z-ADD     1             TBLKUT
050600120831     C                   MOVEL     '3K'          TBLCOD
050700120831     C                   moveL(P)  §PTKSC        TBLKEY
050800120831     C     KTAB2         chain     tabel00f
050900120831     c                   if        %Found(tabel00f)
051000120831     c                   eval      ds3K = tblUNI
051100120831     c                   eval      CLIENTE =  '0' + %editc(§3Kcks:'X')
051200120831     C                   END
051300110330      *>>>>>>>>>>
051400960528     C                   ENDSR
051500120831      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
051600120831      *  Scrive l'OUTPUT da inviare
051700120831      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
051800120831     C     write_OUTPUT  BEGSR
051900120831      *
052000120831     C                   Z-ADD     §MSNUM        STAPRG
052100120831     C                   MOVEL     WDAT          STADAT
052200120831      *
052300120831     C*  aggiorno IFTSTA
052400120831      * Scrittura file per 20/210 Appuntamento da ATR
052500120831     c                   if        STATPR <> 'FINE' and
052600120831     c                             STATPR <> 'RLBK'
052700120831     C                   update    IFTSTA00
052800120831     c                   end
052900120831      *
053000120831     c                   If        §PZPGMTRSS = 'STA94A'
053100120831     c                   exsr      Exit_segmenti
053200120831     c                   elseIf    §PZPGMTRSS = 'GELSTA'
053300120831     c                   exsr      Exit_FileGEL
053400131007     c                   elseIf    §PZPGMTRSS = 'AMZSTA'
053500131007     c                   exsr      Exit_FileAMZ
053600120831     C                   EnD
053700120831      *
053800120831     C                   ENDSR
053900110330      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
054000110330      *  Scrittura dei segmenti IFTSTA D94A
054100110330      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
054200110330     C     Exit_Segmenti BEGSR
054300110330      *
054400110330     c                   eval      Tipo_DS = STATPR
054500110330     c                   eval      Suffisso   = §PUSIG
054600110601     c                   eval      Compatta_80 = §pucompat
054700110330     c                   clear                   DsGenerica
054800110330     c                   eval      DsGenerica = STADAT
054900110330     c                   call      'TRTCT81R1'
055000110330     c                   PARM                    CLIENTE           8
055100110330     c                   PARM                    Suffisso          2
055200110330     c                   PARM                    Tipo_DS           4
055300110330     c                   PARM                    DsGenerica     1024
055400110330     C                   PARM                    Numero_Righe      5 0
055500110601     c                   PARM                    Compatta_80       1
055600110617     C                   PARM                    Progressivo      10
055700110621     c                   PARM                    Esito             1
055800110630     C
055900110630     C                   if        esito = 'E'
056000110630     c                   exsr      Invio_Mail
056100110630     c                   end
056200110330      *
056300110330     C                   ENDSR
056400120831     c*==================================================================*
056500120831      * Manda un File a tracciato NETEXPRESS EUROPE
056600120831     c*==================================================================*
056700120831     c     Exit_FileGEL  begsr
056800120831      *
056900120831     c                   clear                   DsGenerica
057000120831     c                   eval      Tipo_DS = STATPR
057100120831     c                   eval      DsGenerica = STADAT
057200120905     c                   call      'TRTCT81GR'
057300120831     c                   PARM                    CLIENTE
057400120831     c                   PARM                    Tipo_DS           4
057500120831     c                   PARM                    DsGenerica     1024
057600120831     C                   PARM                    Numero_Righe      5 0
057700120831     C                   PARM                    Progressivo      10
057800120831     c                   PARM                    esito             1
057900120831     C
058000120831     C                   if        esito = 'E'
058100120831     c                   exsr      Invio_Mail
058200120831     c                   end
058300120831      *
058400120831     C                   ENDSR
058500131007     c*==================================================================*
058600131007      * Manda un File a tracciato IFTSTA Particolarissimo di AMAZON
058700131007     c*==================================================================*
058800131007     c     Exit_FileAMZ  begsr
058900131007      *
059000131007     c                   clear                   DsGenerica
059100131007     c                   eval      Tipo_DS = STATPR
059200131007     c                   eval      DsGenerica = STADAT
059300131007     c                   call      'TRTCT81AR'
059400131007     c                   PARM                    CLIENTE
059500131007     c                   PARM                    Tipo_DS           4
059600131007     c                   PARM                    DsGenerica     1024
059700131007     C                   PARM                    Numero_Righe      5 0
059800131007     C                   PARM                    Progressivo      10
059900131007     c                   PARM                    esito             1
060000131007     C
060100131007     C                   if        esito = 'E'
060200131007     c                   exsr      Invio_Mail
060300131007     c                   end
060400131007      *
060500131007     C                   ENDSR
060600110630     c*==================================================================*
060700110630      * Manda un Msg x E-mail
060800110630     c*==================================================================*
060900110630     c     Invio_Mail    begsr
061000110630      *
061100120831     c                   clear                   CLIENTEksc        8
061200120831     c                   eval      CLIENTEksc =  '0' + %editc(§PTKSC:'X')
061300110630      *
061400130821???? C                   EVAL      Oggetto ='TRTCT89R:DOWNLD ATR STATI EDI !!'
061500110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione ATR EDI-
061600110630     C                              STATI NON tradotto x invio su DOWNLD. -
061700110630     C                             Controllare il Download del Cliente. << '+
061800170623     c                             CLIENTEksc + ' >>'  + da_dove
061900110630      *
062000110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
062100110630     C* Inizializzo variabili
062200110630     C                   movel     *blanks       wrkEml          253
062300110630     C                   movel     *blanks       wrkMsg         5000
062400110630     C                   movel     *blanks       wrkOgg           44
062500110630     C* Valorizzo i campi della e-m@ail - indirizzo
062600110630     C                   eval      wrkEml = CED_BART
062700110630     C                   eval      wrkOgg = Oggetto
062800110630     C                   eval      wrkMsg = Messaggio
062900110630     C                   call(e)   'TRTCT00R2'
063000110630     C                   parm                    wrkEml
063100110630     C                   parm                    wrkOgg
063200110630     C                   parm                    wrkMsg
063300110630     C*
063400110630     C                   ENDSR
063500110630      * ?------------------------------------------------------------------ */
063600110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
063700110630      * ?------------------------------------------------------------------ */
063800110630     C     *pssr         BEGSR
063900110630     C
064000110630     C                   eval      esito = 'E'
064001170623     C                   movel     'PSSR'        da_dove           4
064100110630     c                   exsr      Invio_Mail
064200110630     C
064300110630     C                   ENDSR     '*CANCL'
064400110630      *  ------------------------------------------------------------------ */
