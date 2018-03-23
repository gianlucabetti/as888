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
018500130821      *
018600130821      *  Solo se ci sono stati precedentemente dei records da scrivere
018700130821     c                   IF        flg_almenouno <> 'N'
018800110330      *
018900110330      *   e deve essere tradotto NON CON INTESA....
019000110330     c                   If        §PZPGMTRSS = 'STA94A'
019100110330     C                               and  Numero_Righe > 1
019200120831     c                               or
019300120831     c                             §PZPGMTRSS = 'GELSTA'
019400120831     C                               and  Numero_Righe > 0
019500120831      *
019600110330      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
019700110330      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
019800110819     C                   clear                   STADAT
019900110819     C                   clear                   wDAT
020000110819     c                   movel     'FINE'        STATPR
020100120831      *
020200120831     c                   exsr      write_OUTPUT
020300110330     c                   eval      Righe_Dett = 'N'
020400110413      *
020500110413     c                   else
020600110413      * deve uscire senza avere fatto nulla
020700110413     c                   eval      STATPR = 'RLBK'
020800120831     c                   exsr      write_OUTPUT
020900110330     c                   end
021000130821      *
021100130821     C                   EndIF
021200110330      *
021300110330     C                   EndIF
021400110330      *
021500110330     c                   EndSR
021600960528     C*----------------------------------------------------------------
021700040707     C*? WRST00 - Aggiorna da ATR a STA normali 781 diventa 784
021800960528     C*----------------------------------------------------------------
021900960528     C     WRST00        BEGSR
022000050902      *
022100050902     C* Caricamento NUMERATORE
022200050902     C                   EXSR      CARNUM
022300960528     C*  Pulisco dati DS
022400960528     C                   CLEAR                   EDST00
022500040707     C                   movel     STADAT        EDST00
022600960528     C*  Codice documento - nome documento
022700040707     C                   MOVEL     '784'         TA1001
022800960705     C                   MOVEL     EDST00        WDAT
022900120831      *  Testata messaggio  UNB-BGM-DTM
023000120831     c                   exsr      write_OUTPUT
023100960528     C*
023200960528     C                   ENDSR
023300960708     C*----------------------------------------------------------------
023400040707     C*? WRST05 - Nulla
023500960708     C*----------------------------------------------------------------
023600960708     C     WRST05        BEGSR
023700960708     C*
023800040708     C*  Pulisco dati DS
023900040708     C                   CLEAR                   EDST05
024000040708     C                   movel     STADAT        EDST05
024100040708     C                   MOVEL     EDST05        WDAT
024200120831      *  Testata messaggio  NAD-SF / NAD-ST
024300120831     c                   exsr      write_OUTPUT
024400040708     C*
024500960708     C                   ENDSR
024600960528     C*----------------------------------------------------------------
024700040707     C*? WRST10 - Nulla
024800960528     C*----------------------------------------------------------------
024900960528     C     WRST10        BEGSR
025000960528     C*
025100040708     C*  Pulisco dati DS
025200040708     C                   CLEAR                   EDST10
025300040708     C                   movel     STADAT        EDST10
025400040708     C                   MOVEL     EDST10        WDAT
025500120831      *  Testata messaggio  RFF-LOC
025600120831     c                   exsr      write_OUTPUT
025700040708     C*
025800960528     C                   ENDSR
025900960528     C*----------------------------------------------------------------
026000040707     C*? WRST15 - nulla
026100960528     C*----------------------------------------------------------------
026200960528     C     WRST15        BEGSR
026300960528     C*
026400040708     C*  Pulisco dati DS
026500040708     C                   CLEAR                   EDST15
026600040708     C                   movel     STADAT        EDST15
026700040708     C                   MOVEL     EDST15        WDAT
026800120831      *  Testata messaggio  FTX
026900120831     c                   exsr      write_OUTPUT
027000040708     C*
027100960528     C                   ENDSR
027200960528     C*----------------------------------------------------------------
027300040707     C*? WRSD00 - Aggancia la bolla x vedere se Supermercato
027400960528     C*----------------------------------------------------------------
027500960528     C     WRSD00        BEGSR
027600040708     C*
027700040708     C* resetta il flag x update o delete
027800040708     c                   move      'N'           flg_update        1
027900040708     C*
028000040708     C*  Pulisco dati DS
028100040708     C                   CLEAR                   EDSD00
028200040708     C                   MOVEL     STADAT        EDSD00
028300040708     C*  Imposto numero spedizione cliente
028400040708     C                   MOVEL     DA1004        CNI_WSV
028500040708     C                   MOVEL     '20'          wsvAAS
028600040708     **
028700040708     ** aggancia la bolla x controllare se supermercato
028800040708     c     kblp          chain     FNblp01l
028900040708     c                   if        %Found(FnBLP01l) and
029000040721     c                             (blpTC1 = 'S' or blpTC2 = 'S' or
029100040721     c                              blpTC1 = 'A' or blpTC2 = 'A')
029200040708     c                   move      'S'           flg_update
029300040708     c                   end
029400120831      *
029500040708     c                   if        flg_update = 'S'
029600120831     c                   move      'S'           flg_almenouno
029700040708     C                   ADD       1             WPROG
029800040708     C                   z-add     WPROG         DA1490
029900040708     C                   MOVEL     EDSD00        WDAT
030000120831      *  Testata messaggio  CNI-
030100120831     c                   exsr      write_OUTPUT
030200110330     C*
030300040708     c                   else
030400040708     C                   delete    IFTSTA00
030500040708     c                   end
030600960528     C*
030700960528     C                   ENDSR
030800960528     C*----------------------------------------------------------------
030900960528     C*? WRSD05 - Scrittura record partner - dettaglio
031000960528     C*----------------------------------------------------------------
031100960528     C     WRSD05        BEGSR
031200960528     C*
031300040708     C*  Pulisco dati DS
031400040708     C                   CLEAR                   EDSD05
031500040708     C                   MOVEL     STADAT        EDSD05
031600040708      *
031700040708      *  20+210 --> segnala se consegnata a un Supermercato
031800040708     C                   MOVEL     '20 '         DB9011
031900040708     C                   MOVEL     '210'         DB9013
032000120831      **
032100040708     c                   if        flg_update = 'S'
032200040708     C                   MOVEL     EDSD05        WDAT
032300120831      *  Testata messaggio  STS-RFF-DTM
032400120831     c                   exsr      write_OUTPUT
032500110330      *
032600110330     C                   MOVEL     'S'           WRTEVE            1
032700110330     c                   eval      Righe_Dett = 'S'
032800040708     c                   else
032900120831      *
033000040708     C                   delete    IFTSTA00
033100040708     c                   end
033200961010     C*
033300040708     C                   ENDSR
033400960528     C*-------------------------------------------------------------*
033500960528     C*? WRSD10 - Scrittura record partner - dettaglio
033600960528     C*----------------------------------------------------------------
033700960528     C     WRSD10        BEGSR
033800040708     C*
033900040708     C*  Pulisco dati DS
034000040708     C                   CLEAR                   EDSD10
034100040708     C                   MOVEL     STADAT        EDSD10
034200040708      *
034300040708     c                   if        flg_update = 'S'
034400040708     C                   MOVEL     EDSD10        WDAT
034500120831      *  Testata messaggio  NAD
034600120831     c                   exsr      write_OUTPUT
034700110330      *
034800040708     c                   else
034900040708     C                   delete    IFTSTA00
035000040708     c                   end
035100040708     C*
035200960528     C                   ENDSR
035300960528     C*-------------------------------------------------------------*
035400960528     C*? WRSD15 - Scrittura record partner - dettaglio
035500960528     C*----------------------------------------------------------------
035600960528     C     WRSD15        BEGSR
035700960528     C*
035800040708     C*  Pulisco dati DS
035900040708     C                   CLEAR                   EDSD15
036000040708     C                   MOVEL     STADAT        EDSD15
036100120831      *
036200040708     c                   if        flg_update = 'S'
036300040708     C                   MOVEL     EDSD15        WDAT
036400120831      *  Testata messaggio  FTX-
036500120831     c                   exsr      write_OUTPUT
036600110330     c                   eval      Righe_Dett = 'S'
036700120831      *
036800040708     c                   else
036900040708     C                   delete    IFTSTA00
037000040708     c                   end
037100040708     C*
037200960528     C                   ENDSR
037300960528     C*-------------------------------------------------------------*
037400960528     C*? WRSD20 - Scrittura record partner - dettaglio
037500960528     C*----------------------------------------------------------------
037600960528     C     WRSD20        BEGSR
037700960912     C*
037800040708     C*  Pulisco dati DS
037900040708     C                   CLEAR                   EDSD20
038000040708     C                   MOVEL     STADAT        EDSD20
038100120831     C*
038200040708     c                   if        flg_update = 'S'
038300040708     C                   MOVEL     EDSD20        WDAT
038400120831      *  Testata messaggio  GID-PCI
038500120831     c                   exsr      write_OUTPUT
038600110330     c                   eval      Righe_Dett = 'S'
038700110330     C*
038800040708     c                   else
038900040708     C                   delete    IFTSTA00
039000040708     c                   end
039100960912     C*
039200960912     C                   ENDSR
039300960730     C*----------------------------------------------------------------
039400960927     C*? CARNUM - CARICAMENTO NUMERATORE
039500960927     C*----------------------------------------------------------------
039600960927     C     CARNUM        BEGSR
039700960927     C*
039800960927     C                   Z-ADD     0             WPROG             4 0
039900960927     C                   Z-ADD     0             X                 3 0
040000960927     C                   Z-ADD     1             §MSNUM
040100961010     C                   MOVEL     'MS'          KCOD
040200961010     C                   MOVEL     *BLANKS       KKEY
040300040707     C                   MOVEL     '784'         KKEY
040400961010     C     KTAB          CHAIN     EDTAB01L                           30
040500960927     C     *IN30         IFEQ      '0'
040600960927     C     TABFLG        ANDEQ     *BLANKS
040700960927     C                   MOVEL     TABUNI        DSMS
040800960927     C                   ADD       1             §MSNUM
040900061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
041000061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
041100061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
041200061219     c                   move      §MSNUM        campo_4n          4 0
041300061219     c                   if        campo_4n = 0
041400061219     c                   add       1             §MSNUM
041500061219     c                   end
041600960927     C                   Z-ADD     WOGGI         §MSDAT
041700960927     C                   MOVEL     DSMS          TABUNI
041800960927     C                   UPDATE    EDTAB000
041900960927     C                   END
042000960927     C*
042100960927     C                   ENDSR
042200960528     C*----------------------------------------------------------------
042300960528     C*? *INZSR - OPERAZIONI INIZIALI
042400960528     C*----------------------------------------------------------------
042500960528     C     *INZSR        BEGSR
042600960530     C*
042700960530     C     *ENTRY        PLIST
042800960530     C                   PARM                    KPJBA
042900110630     C                   PARM                    Esito             1            ritorno errore
043000110330     C                   MOVEL     KPJBU         TRTC87
043100110601      *
043200110601      * reset campo compattazione x Download file
043300110630     C                   eval      esito = '0'
043400110601     c                   eval      Compatta_80 = 'S'
043500040708     C*
043600960528     C* Definisco chiavi di accesso
043700961010     C     KTAB          KLIST
043800961010     C                   KFLD                    KCOD
043900961010     C                   KFLD                    KKEY
044000040707     **
044100040708     C     Kblp          KLIST
044200040708     C                   KFLD                    WsvAAS
044300040708     C                   KFLD                    WsvLNP
044400040708     C                   KFLD                    WsvNRS
044500040708     C                   KFLD                    WsvNSP
044600120831      *
044700120831      * DEVE ESSERE PILOTATO DALLA TAB.3K
044800120831     C     KTAB2         KLIST
044900120831     C                   KFLD                    TBLKUT
045000120831     C                   KFLD                    TBLCOD
045100120831     C                   KFLD                    TBLKEY
045200040708     **
045300980330     C     *LIKE         DEFINE    TABCOD        KCOD
045400961010     C     *LIKE         DEFINE    TABKEY        KKEY
045500960528     C*
045600960528     C* Recupero codice AS
045700960528     C                   Z-ADD     1             CODUT             1 0
045800960705     C                   CALL      'X§PARUT'
045900960528     C                   PARM                    UTEDSE
046000960528     C                   MOVEL     REC80         CNCR80
046100980622     C                   MOVEL     '005'         WAS400            3
046200040708     C*
046300960528     C* Recupero data e ora x messaggio
046400960528     C                   TIME                    WHHDAT           14 0
046500960708     C                   MOVE      WHHDAT        G02DAT
046600960528     C                   MOVE      WHHDAT        DATA              6 0
046700960528     C                   MOVEL     *BLANK        G02ERR
046800960528     C                   CALL      'XSRDA8'
046900960528     C                   PARM                    WLBDA8
047000960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
047100960708     C                   MOVEL     WHHDAT        WHHMM             4 0
047200960528     C                   MOVE      WOGGI         WOGGI6            6 0
047300960528     C                   MOVE      WHHDAT        WANNO             2 0
047400960528     C                   MOVEL     WANNO         WOGGI6
047500040708     C*
047600960528     C* Caricamento Tabella Partner esteri
047700960528     C                   Z-ADD     0             X                 3 0
047800110330     C                   MOVEL     'PT'          KCOD
047900110330     C                   MOVEL     D87CPT        KKEY
048000120831     C                   clear                   DSPT
048100110330     C     KTAB          CHAIN     EDTAB01L                           30
048200110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
048300110330     C                   MOVEL     TABUNI        DSPT
048400110330     C                   END
048500110330     C                   MOVEL     'PU'          KCOD
048600120831     C                   clear                   DSPU
048700110330     C     KTAB          CHAIN     EDTAB01L                           30
048800110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
048900110330     C                   MOVEL     TABUNI        DSPU
049000110330     C                   END
049100110330      *   Con quale traduttore deve essere scaricato e scritto il File ????
049200110330     C                   MOVEL     'PZ'          KCOD
049300120831     C                   clear                   DSPZ
049400110330     C     KTAB          CHAIN     EDTAB01L
049500110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
049600110330     C                   MOVEL     TABUNI        DSPZ
049700110330     C                   END
049800120831      *
049900120831     c                   eval      CLIENTE =  '0' + %editc(§PTKSC:'X')
050000120831     C                   Z-ADD     1             TBLKUT
050100120831     C                   MOVEL     '3K'          TBLCOD
050200120831     C                   moveL(P)  §PTKSC        TBLKEY
050300120831     C     KTAB2         chain     tabel00f
050400120831     c                   if        %Found(tabel00f)
050500120831     c                   eval      ds3K = tblUNI
050600120831     c                   eval      CLIENTE =  '0' + %editc(§3Kcks:'X')
050700120831     C                   END
050800110330      *>>>>>>>>>>
050900960528     C                   ENDSR
051000120831      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
051100120831      *  Scrive l'OUTPUT da inviare
051200120831      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
051300120831     C     write_OUTPUT  BEGSR
051400120831      *
051500120831     C                   Z-ADD     §MSNUM        STAPRG
051600120831     C                   MOVEL     WDAT          STADAT
051700120831      *
051800120831     C*  aggiorno IFTSTA
051900120831      * Scrittura file per 20/210 Appuntamento da ATR
052000120831     c                   if        STATPR <> 'FINE' and
052100120831     c                             STATPR <> 'RLBK'
052200120831     C                   update    IFTSTA00
052300120831     c                   end
052400120831      *
052500120831     c                   If        §PZPGMTRSS = 'STA94A'
052600120831     c                   exsr      Exit_segmenti
052700120831     c                   elseIf    §PZPGMTRSS = 'GELSTA'
052800120831     c                   exsr      Exit_FileGEL
052900120831     C                   EnD
053000120831      *
053100120831     C                   ENDSR
053200110330      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
053300110330      *  Scrittura dei segmenti IFTSTA D94A
053400110330      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
053500110330     C     Exit_Segmenti BEGSR
053600110330      *
053700110330     c                   eval      Tipo_DS = STATPR
053800110330     c                   eval      Suffisso   = §PUSIG
053900110601     c                   eval      Compatta_80 = §pucompat
054000110330     c                   clear                   DsGenerica
054100110330     c                   eval      DsGenerica = STADAT
054200110330     c                   call      'TRTCT81R1'
054300110330     c                   PARM                    CLIENTE           8
054400110330     c                   PARM                    Suffisso          2
054500110330     c                   PARM                    Tipo_DS           4
054600110330     c                   PARM                    DsGenerica     1024
054700110330     C                   PARM                    Numero_Righe      5 0
054800110601     c                   PARM                    Compatta_80       1
054900110617     C                   PARM                    Progressivo      10
055000110621     c                   PARM                    Esito             1
055100110630     C
055200110630     C                   if        esito = 'E'
055300110630     c                   exsr      Invio_Mail
055400110630     c                   end
055500110330      *
055600110330     C                   ENDSR
055700120831     c*==================================================================*
055800120831      * Manda un File a tracciato NETEXPRESS EUROPE
055900120831     c*==================================================================*
056000120831     c     Exit_FileGEL  begsr
056100120831      *
056200120831     c                   clear                   DsGenerica
056300120831     c                   eval      Tipo_DS = STATPR
056400120831     c                   eval      DsGenerica = STADAT
056500120905     c                   call      'TRTCT81GR'
056600120831     c                   PARM                    CLIENTE
056700120831     c                   PARM                    Tipo_DS           4
056800120831     c                   PARM                    DsGenerica     1024
056900120831     C                   PARM                    Numero_Righe      5 0
057000120831     C                   PARM                    Progressivo      10
057100120831     c                   PARM                    esito             1
057200120831     C
057300120831     C                   if        esito = 'E'
057400120831     c                   exsr      Invio_Mail
057500120831     c                   end
057600120831      *
057700120831     C                   ENDSR
057800110630     c*==================================================================*
057900110630      * Manda un Msg x E-mail
058000110630     c*==================================================================*
058100110630     c     Invio_Mail    begsr
058200110630      *
058300120831     c                   clear                   CLIENTEksc        8
058400120831     c                   eval      CLIENTEksc =  '0' + %editc(§PTKSC:'X')
058500110630      *
058600130821???? C                   EVAL      Oggetto ='TRTCT89R:DOWNLD ATR STATI EDI !!'
058700110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione ATR EDI-
058800110630     C                              STATI NON tradotto x invio su DOWNLD. -
058900110630     C                             Controllare il Download del Cliente. << '+
059000120831     c                             CLIENTEksc + ' >>'
059100110630      *
059200110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
059300110630     C* Inizializzo variabili
059400110630     C                   movel     *blanks       wrkEml          253
059500110630     C                   movel     *blanks       wrkMsg         5000
059600110630     C                   movel     *blanks       wrkOgg           44
059700110630     C* Valorizzo i campi della e-m@ail - indirizzo
059800110630     C                   eval      wrkEml = CED_BART
059900110630     C                   eval      wrkOgg = Oggetto
060000110630     C                   eval      wrkMsg = Messaggio
060100110630     C                   call(e)   'TRTCT00R2'
060200110630     C                   parm                    wrkEml
060300110630     C                   parm                    wrkOgg
060400110630     C                   parm                    wrkMsg
060500110630     C*
060600110630     C                   ENDSR
060700110630      * ?------------------------------------------------------------------ */
060800110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
060900110630      * ?------------------------------------------------------------------ */
061000110630     C     *pssr         BEGSR
061100110630     C
061200110630     C                   eval      esito = 'E'
061300110630     c                   exsr      Invio_Mail
061400110630     C
061500110630     C                   ENDSR     '*CANCL'
061600110630      *  ------------------------------------------------------------------ */
