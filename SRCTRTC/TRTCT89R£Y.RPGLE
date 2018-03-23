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
017300121010     c                   eval      esito = 'N'
017400121010     c                   end
017500121010      *
017600960528     C                   SETON                                        LR
017700110330     C*----------------------------------------------------------------
017800110330     C*? x Fine File
017900110330     C*----------------------------------------------------------------
018000110418     C     EoF_EDSTA00F  BEGSR
018100110330      *
018200110330     C* se finito il File EoF
018300110418     C                   If          %eof(EDSTA00F)
018400110330     C                   MOVEL     'S'           WFINE             1
018500110330      *
018600110330      *   e deve essere tradotto NON CON INTESA....
018700110330     c                   If        §PZPGMTRSS = 'STA94A'
018800110330     C                               and  Numero_Righe > 1
018900120831     c                               or
019000120831     c                             §PZPGMTRSS = 'GELSTA'
019100120831     C                               and  Numero_Righe > 0
019200120831      *
019300110330      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
019400110330      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
019500110819     C                   clear                   STADAT
019600110819     C                   clear                   wDAT
019700110819     c                   movel     'FINE'        STATPR
019800120831      *
019900120831     c                   exsr      write_OUTPUT
020000110330     c                   eval      Righe_Dett = 'N'
020100110413      *
020200110413     c                   else
020300110413      * deve uscire senza avere fatto nulla
020400110413     c                   eval      STATPR = 'RLBK'
020500120831     c                   exsr      write_OUTPUT
020600110330     c                   end
020700110330      *
020800110330     C                   EndIF
020900110330      *
021000110330     c                   EndSR
021100960528     C*----------------------------------------------------------------
021200040707     C*? WRST00 - Aggiorna da ATR a STA normali 781 diventa 784
021300960528     C*----------------------------------------------------------------
021400960528     C     WRST00        BEGSR
021500050902      *
021600050902     C* Caricamento NUMERATORE
021700050902     C                   EXSR      CARNUM
021800960528     C*  Pulisco dati DS
021900960528     C                   CLEAR                   EDST00
022000040707     C                   movel     STADAT        EDST00
022100960528     C*  Codice documento - nome documento
022200040707     C                   MOVEL     '784'         TA1001
022300960705     C                   MOVEL     EDST00        WDAT
022400120831      *  Testata messaggio  UNB-BGM-DTM
022500120831     c                   exsr      write_OUTPUT
022600960528     C*
022700960528     C                   ENDSR
022800960708     C*----------------------------------------------------------------
022900040707     C*? WRST05 - Nulla
023000960708     C*----------------------------------------------------------------
023100960708     C     WRST05        BEGSR
023200960708     C*
023300040708     C*  Pulisco dati DS
023400040708     C                   CLEAR                   EDST05
023500040708     C                   movel     STADAT        EDST05
023600040708     C                   MOVEL     EDST05        WDAT
023700120831      *  Testata messaggio  NAD-SF / NAD-ST
023800120831     c                   exsr      write_OUTPUT
023900040708     C*
024000960708     C                   ENDSR
024100960528     C*----------------------------------------------------------------
024200040707     C*? WRST10 - Nulla
024300960528     C*----------------------------------------------------------------
024400960528     C     WRST10        BEGSR
024500960528     C*
024600040708     C*  Pulisco dati DS
024700040708     C                   CLEAR                   EDST10
024800040708     C                   movel     STADAT        EDST10
024900040708     C                   MOVEL     EDST10        WDAT
025000120831      *  Testata messaggio  RFF-LOC
025100120831     c                   exsr      write_OUTPUT
025200040708     C*
025300960528     C                   ENDSR
025400960528     C*----------------------------------------------------------------
025500040707     C*? WRST15 - nulla
025600960528     C*----------------------------------------------------------------
025700960528     C     WRST15        BEGSR
025800960528     C*
025900040708     C*  Pulisco dati DS
026000040708     C                   CLEAR                   EDST15
026100040708     C                   movel     STADAT        EDST15
026200040708     C                   MOVEL     EDST15        WDAT
026300120831      *  Testata messaggio  FTX
026400120831     c                   exsr      write_OUTPUT
026500040708     C*
026600960528     C                   ENDSR
026700960528     C*----------------------------------------------------------------
026800040707     C*? WRSD00 - Aggancia la bolla x vedere se Supermercato
026900960528     C*----------------------------------------------------------------
027000960528     C     WRSD00        BEGSR
027100040708     C*
027200040708     C* resetta il flag x update o delete
027300040708     c                   move      'N'           flg_update        1
027400040708     C*
027500040708     C*  Pulisco dati DS
027600040708     C                   CLEAR                   EDSD00
027700040708     C                   MOVEL     STADAT        EDSD00
027800040708     C*  Imposto numero spedizione cliente
027900040708     C                   MOVEL     DA1004        CNI_WSV
028000040708     C                   MOVEL     '20'          wsvAAS
028100040708     **
028200040708     ** aggancia la bolla x controllare se supermercato
028300040708     c     kblp          chain     FNblp01l
028400040708     c                   if        %Found(FnBLP01l) and
028500040721     c                             (blpTC1 = 'S' or blpTC2 = 'S' or
028600040721     c                              blpTC1 = 'A' or blpTC2 = 'A')
028700040708     c                   move      'S'           flg_update
028800040708     c                   end
028900120831      *
029000040708     c                   if        flg_update = 'S'
029100120831     c                   move      'S'           flg_almenouno
029200040708     C                   ADD       1             WPROG
029300040708     C                   z-add     WPROG         DA1490
029400040708     C                   MOVEL     EDSD00        WDAT
029500120831      *  Testata messaggio  CNI-
029600120831     c                   exsr      write_OUTPUT
029700110330     C*
029800040708     c                   else
029900040708     C                   delete    IFTSTA00
030000040708     c                   end
030100960528     C*
030200960528     C                   ENDSR
030300960528     C*----------------------------------------------------------------
030400960528     C*? WRSD05 - Scrittura record partner - dettaglio
030500960528     C*----------------------------------------------------------------
030600960528     C     WRSD05        BEGSR
030700960528     C*
030800040708     C*  Pulisco dati DS
030900040708     C                   CLEAR                   EDSD05
031000040708     C                   MOVEL     STADAT        EDSD05
031100040708      *
031200040708      *  20+210 --> segnala se consegnata a un Supermercato
031300040708     C                   MOVEL     '20 '         DB9011
031400040708     C                   MOVEL     '210'         DB9013
031500120831      **
031600040708     c                   if        flg_update = 'S'
031700040708     C                   MOVEL     EDSD05        WDAT
031800120831      *  Testata messaggio  STS-RFF-DTM
031900120831     c                   exsr      write_OUTPUT
032000110330      *
032100110330     C                   MOVEL     'S'           WRTEVE            1
032200110330     c                   eval      Righe_Dett = 'S'
032300040708     c                   else
032400120831      *
032500040708     C                   delete    IFTSTA00
032600040708     c                   end
032700961010     C*
032800040708     C                   ENDSR
032900960528     C*-------------------------------------------------------------*
033000960528     C*? WRSD10 - Scrittura record partner - dettaglio
033100960528     C*----------------------------------------------------------------
033200960528     C     WRSD10        BEGSR
033300040708     C*
033400040708     C*  Pulisco dati DS
033500040708     C                   CLEAR                   EDSD10
033600040708     C                   MOVEL     STADAT        EDSD10
033700040708      *
033800040708     c                   if        flg_update = 'S'
033900040708     C                   MOVEL     EDSD10        WDAT
034000120831      *  Testata messaggio  NAD
034100120831     c                   exsr      write_OUTPUT
034200110330      *
034300040708     c                   else
034400040708     C                   delete    IFTSTA00
034500040708     c                   end
034600040708     C*
034700960528     C                   ENDSR
034800960528     C*-------------------------------------------------------------*
034900960528     C*? WRSD15 - Scrittura record partner - dettaglio
035000960528     C*----------------------------------------------------------------
035100960528     C     WRSD15        BEGSR
035200960528     C*
035300040708     C*  Pulisco dati DS
035400040708     C                   CLEAR                   EDSD15
035500040708     C                   MOVEL     STADAT        EDSD15
035600120831      *
035700040708     c                   if        flg_update = 'S'
035800040708     C                   MOVEL     EDSD15        WDAT
035900120831      *  Testata messaggio  FTX-
036000120831     c                   exsr      write_OUTPUT
036100110330     c                   eval      Righe_Dett = 'S'
036200120831      *
036300040708     c                   else
036400040708     C                   delete    IFTSTA00
036500040708     c                   end
036600040708     C*
036700960528     C                   ENDSR
036800960528     C*-------------------------------------------------------------*
036900960528     C*? WRSD20 - Scrittura record partner - dettaglio
037000960528     C*----------------------------------------------------------------
037100960528     C     WRSD20        BEGSR
037200960912     C*
037300040708     C*  Pulisco dati DS
037400040708     C                   CLEAR                   EDSD20
037500040708     C                   MOVEL     STADAT        EDSD20
037600120831     C*
037700040708     c                   if        flg_update = 'S'
037800040708     C                   MOVEL     EDSD20        WDAT
037900120831      *  Testata messaggio  GID-PCI
038000120831     c                   exsr      write_OUTPUT
038100110330     c                   eval      Righe_Dett = 'S'
038200110330     C*
038300040708     c                   else
038400040708     C                   delete    IFTSTA00
038500040708     c                   end
038600960912     C*
038700960912     C                   ENDSR
038800960730     C*----------------------------------------------------------------
038900960927     C*? CARNUM - CARICAMENTO NUMERATORE
039000960927     C*----------------------------------------------------------------
039100960927     C     CARNUM        BEGSR
039200960927     C*
039300960927     C                   Z-ADD     0             WPROG             4 0
039400960927     C                   Z-ADD     0             X                 3 0
039500960927     C                   Z-ADD     1             §MSNUM
039600961010     C                   MOVEL     'MS'          KCOD
039700961010     C                   MOVEL     *BLANKS       KKEY
039800040707     C                   MOVEL     '784'         KKEY
039900961010     C     KTAB          CHAIN     EDTAB01L                           30
040000960927     C     *IN30         IFEQ      '0'
040100960927     C     TABFLG        ANDEQ     *BLANKS
040200960927     C                   MOVEL     TABUNI        DSMS
040300960927     C                   ADD       1             §MSNUM
040400061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
040500061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
040600061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
040700061219     c                   move      §MSNUM        campo_4n          4 0
040800061219     c                   if        campo_4n = 0
040900061219     c                   add       1             §MSNUM
041000061219     c                   end
041100960927     C                   Z-ADD     WOGGI         §MSDAT
041200960927     C                   MOVEL     DSMS          TABUNI
041300960927     C                   UPDATE    EDTAB000
041400960927     C                   END
041500960927     C*
041600960927     C                   ENDSR
041700960528     C*----------------------------------------------------------------
041800960528     C*? *INZSR - OPERAZIONI INIZIALI
041900960528     C*----------------------------------------------------------------
042000960528     C     *INZSR        BEGSR
042100960530     C*
042200960530     C     *ENTRY        PLIST
042300960530     C                   PARM                    KPJBA
042400110630     C                   PARM                    Esito             1            ritorno errore
042500110330     C                   MOVEL     KPJBU         TRTC87
042600110601      *
042700110601      * reset campo compattazione x Download file
042800110630     C                   eval      esito = '0'
042900110601     c                   eval      Compatta_80 = 'S'
043000040708     C*
043100960528     C* Definisco chiavi di accesso
043200961010     C     KTAB          KLIST
043300961010     C                   KFLD                    KCOD
043400961010     C                   KFLD                    KKEY
043500040707     **
043600040708     C     Kblp          KLIST
043700040708     C                   KFLD                    WsvAAS
043800040708     C                   KFLD                    WsvLNP
043900040708     C                   KFLD                    WsvNRS
044000040708     C                   KFLD                    WsvNSP
044100120831      *
044200120831      * DEVE ESSERE PILOTATO DALLA TAB.3K
044300120831     C     KTAB2         KLIST
044400120831     C                   KFLD                    TBLKUT
044500120831     C                   KFLD                    TBLCOD
044600120831     C                   KFLD                    TBLKEY
044700040708     **
044800980330     C     *LIKE         DEFINE    TABCOD        KCOD
044900961010     C     *LIKE         DEFINE    TABKEY        KKEY
045000960528     C*
045100960528     C* Recupero codice AS
045200960528     C                   Z-ADD     1             CODUT             1 0
045300960705     C                   CALL      'X§PARUT'
045400960528     C                   PARM                    UTEDSE
045500960528     C                   MOVEL     REC80         CNCR80
045600980622     C                   MOVEL     '005'         WAS400            3
045700040708     C*
045800960528     C* Recupero data e ora x messaggio
045900960528     C                   TIME                    WHHDAT           14 0
046000960708     C                   MOVE      WHHDAT        G02DAT
046100960528     C                   MOVE      WHHDAT        DATA              6 0
046200960528     C                   MOVEL     *BLANK        G02ERR
046300960528     C                   CALL      'XSRDA8'
046400960528     C                   PARM                    WLBDA8
046500960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
046600960708     C                   MOVEL     WHHDAT        WHHMM             4 0
046700960528     C                   MOVE      WOGGI         WOGGI6            6 0
046800960528     C                   MOVE      WHHDAT        WANNO             2 0
046900960528     C                   MOVEL     WANNO         WOGGI6
047000040708     C*
047100960528     C* Caricamento Tabella Partner esteri
047200960528     C                   Z-ADD     0             X                 3 0
047300110330     C                   MOVEL     'PT'          KCOD
047400110330     C                   MOVEL     D87CPT        KKEY
047500120831     C                   clear                   DSPT
047600110330     C     KTAB          CHAIN     EDTAB01L                           30
047700110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
047800110330     C                   MOVEL     TABUNI        DSPT
047900110330     C                   END
048000110330     C                   MOVEL     'PU'          KCOD
048100120831     C                   clear                   DSPU
048200110330     C     KTAB          CHAIN     EDTAB01L                           30
048300110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
048400110330     C                   MOVEL     TABUNI        DSPU
048500110330     C                   END
048600110330      *   Con quale traduttore deve essere scaricato e scritto il File ????
048700110330     C                   MOVEL     'PZ'          KCOD
048800120831     C                   clear                   DSPZ
048900110330     C     KTAB          CHAIN     EDTAB01L
049000110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
049100110330     C                   MOVEL     TABUNI        DSPZ
049200110330     C                   END
049300120831      *
049400120831     c                   eval      CLIENTE =  '0' + %editc(§PTKSC:'X')
049500120831     C                   Z-ADD     1             TBLKUT
049600120831     C                   MOVEL     '3K'          TBLCOD
049700120831     C                   moveL(P)  §PTKSC        TBLKEY
049800120831     C     KTAB2         chain     tabel00f
049900120831     c                   if        %Found(tabel00f)
050000120831     c                   eval      ds3K = tblUNI
050100120831     c                   eval      CLIENTE =  '0' + %editc(§3Kcks:'X')
050200120831     C                   END
050300110330      *>>>>>>>>>>
050400960528     C                   ENDSR
050500120831      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
050600120831      *  Scrive l'OUTPUT da inviare
050700120831      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
050800120831     C     write_OUTPUT  BEGSR
050900120831      *
051000120831     C                   Z-ADD     §MSNUM        STAPRG
051100120831     C                   MOVEL     WDAT          STADAT
051200120831      *
051300120831     C*  aggiorno IFTSTA
051400120831      * Scrittura file per 20/210 Appuntamento da ATR
051500120831     c                   if        STATPR <> 'FINE' and
051600120831     c                             STATPR <> 'RLBK'
051700120831     C                   update    IFTSTA00
051800120831     c                   end
051900120831      *
052000120831     c                   If        §PZPGMTRSS = 'STA94A'
052100120831     c                   exsr      Exit_segmenti
052200120831     c                   elseIf    §PZPGMTRSS = 'GELSTA'
052300120831     c                   exsr      Exit_FileGEL
052400120831     C                   EnD
052500120831      *
052600120831     C                   ENDSR
052700110330      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
052800110330      *  Scrittura dei segmenti IFTSTA D94A
052900110330      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
053000110330     C     Exit_Segmenti BEGSR
053100110330      *
053200110330     c                   eval      Tipo_DS = STATPR
053300110330     c                   eval      Suffisso   = §PUSIG
053400110601     c                   eval      Compatta_80 = §pucompat
053500110330     c                   clear                   DsGenerica
053600110330     c                   eval      DsGenerica = STADAT
053700110330     c                   call      'TRTCT81R1'
053800110330     c                   PARM                    CLIENTE           8
053900110330     c                   PARM                    Suffisso          2
054000110330     c                   PARM                    Tipo_DS           4
054100110330     c                   PARM                    DsGenerica     1024
054200110330     C                   PARM                    Numero_Righe      5 0
054300110601     c                   PARM                    Compatta_80       1
054400110617     C                   PARM                    Progressivo      10
054500110621     c                   PARM                    Esito             1
054600110630     C
054700110630     C                   if        esito = 'E'
054800110630     c                   exsr      Invio_Mail
054900110630     c                   end
055000110330      *
055100110330     C                   ENDSR
055200120831     c*==================================================================*
055300120831      * Manda un File a tracciato NETEXPRESS EUROPE
055400120831     c*==================================================================*
055500120831     c     Exit_FileGEL  begsr
055600120831      *
055700120831     c                   clear                   DsGenerica
055800120831     c                   eval      Tipo_DS = STATPR
055900120831     c                   eval      DsGenerica = STADAT
056000120905     c                   call      'TRTCT81GR'
056100120831     c                   PARM                    CLIENTE
056200120831     c                   PARM                    Tipo_DS           4
056300120831     c                   PARM                    DsGenerica     1024
056400120831     C                   PARM                    Numero_Righe      5 0
056500120831     C                   PARM                    Progressivo      10
056600120831     c                   PARM                    esito             1
056700120831     C
056800120831     C                   if        esito = 'E'
056900120831     c                   exsr      Invio_Mail
057000120831     c                   end
057100120831      *
057200120831     C                   ENDSR
057300110630     c*==================================================================*
057400110630      * Manda un Msg x E-mail
057500110630     c*==================================================================*
057600110630     c     Invio_Mail    begsr
057700110630      *
057800120831     c                   clear                   CLIENTEksc        8
057900120831     c                   eval      CLIENTEksc =  '0' + %editc(§PTKSC:'X')
058000110630      *
058100110630???? C                   EVAL      Oggetto ='Problemi DOWNLD ATR STATI EDI !!'
058200110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione ATR EDI-
058300110630     C                              STATI NON tradotto x invio su DOWNLD. -
058400110630     C                             Controllare il Download del Cliente. << '+
058500120831     c                             CLIENTEksc + ' >>'
058600110630      *
058700110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
058800110630     C* Inizializzo variabili
058900110630     C                   movel     *blanks       wrkEml          253
059000110630     C                   movel     *blanks       wrkMsg         5000
059100110630     C                   movel     *blanks       wrkOgg           44
059200110630     C* Valorizzo i campi della e-m@ail - indirizzo
059300110630     C                   eval      wrkEml = CED_BART
059400110630     C                   eval      wrkOgg = Oggetto
059500110630     C                   eval      wrkMsg = Messaggio
059600110630     C                   call(e)   'TRTCT00R2'
059700110630     C                   parm                    wrkEml
059800110630     C                   parm                    wrkOgg
059900110630     C                   parm                    wrkMsg
060000110630     C*
060100110630     C                   ENDSR
060200110630      * ?------------------------------------------------------------------ */
060300110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
060400110630      * ?------------------------------------------------------------------ */
060500110630     C     *pssr         BEGSR
060600110630     C
060700110630     C                   eval      esito = 'E'
060800110630     c                   exsr      Invio_Mail
060900110630     C
061000110630     C                   ENDSR     '*CANCL'
061100110630      *  ------------------------------------------------------------------ */
