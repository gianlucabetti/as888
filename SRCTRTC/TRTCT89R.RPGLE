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
001400170707     FFIAR403L  IF   E           K DISK
001500960528     D*---------------------------------------------------------------------*
001600960528     D* Schiere
001700960528     D*---------------------------------------------------------------------*
001800960528     D* Schiera per scrittura dati località record ST10
001900960528     D* Schiera per scrittura testi liberi record ST15
002000960528     D* Schiera per scrittura testi liberi record SD15
002100960528     D KSC             S              7  0 DIM(100)
002200010409     D FTT             S              1    DIM(100)
002300960821     D* TESTO LIBERO
002400960821     D FTX             S             70    DIM(1) CTDATA PERRCD(1)
002500960912     D* composizione schiera 35 chr x segnacollo
002600960912     D C35             S              1    DIM(35)
002700960528     D*---------------------------------------------------------------------*
002800960528     D* DS
002900960528     D*---------------------------------------------------------------------*
003000960528     D* .. per scompozione dati da spedire a seconda del tipo record
003100960528     D EDST00        E DS                  EXTNAME(EDST00DS)
003200960708     D EDST05        E DS                  EXTNAME(EDST05DS)
003300960528     D EDST10        E DS                  EXTNAME(EDST10DS)
003400960528     D  T10                   48    103
003500960528     D                                     DIM(2)
003600960528     D EDST15        E DS                  EXTNAME(EDST15DS)
003700960528     D  T15                   13    362
003800960528     D                                     DIM(5)
003900960528     D EDSD00        E DS                  EXTNAME(EDSD00DS)
004000960528     D EDSD05        E DS                  EXTNAME(EDSD05DS)
004100960528     D EDSD10        E DS                  EXTNAME(EDSD10DS)
004200960528     D EDSD15        E DS                  EXTNAME(EDSD15DS)
004300960528     D  D15                   13    362
004400960528     D                                     DIM(5)
004500960528     D EDSD20        E DS                  EXTNAME(EDSD20DS)
004600960730     D  D20                   33    732
004700960730     D                                     DIM(20)
004800960528     D*---------------------------------------------------------------------*
004900960528     D* .. per reperimento tipo record di EDIFTSTA da scrivere
005000960528     D*                    .. 5 - 8  : tipo record
005100040218     D WDAT            DS          1950
005200960729     D  STAPRG                 1      4  0
005300960726     D  STATPR                 5      8
005400960528     D*---------------------------------------------------------------------*
005500960528     D KPJBA         E DS
005600960528     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
005700960528     D CNCR80        E DS
005800120831     D DS3K          E DS
005900110330     D DSPT          E DS                  EXTNAME(EDIDSPT)
006000110330     D DSPU          E DS                  EXTNAME(EDIDSPU)
006100110330     D DSPZ          E DS                  EXTNAME(EDIDSPZ)
006200110330     D DSPW          E DS                  EXTNAME(EDIDSPW)
006300110330     D DSPV          E DS                  EXTNAME(EDIDSPV)
006400960726     D DSMS          E DS                  EXTNAME(EDIDSMS)
006500110330     D TRTC87        E DS                  EXTNAME(TRTC87DS)
006600110330     d Righe_Dett      s              1
006700961010     D*  DS x rottura livello spedizione - codice errore
006800961010     D SAVSPE          DS
006900040305     D  CNI_WSV                3     16  0
007000961010     D  WSVAAS                 1      4  0
007100961010     D  WSVLNP                 5      7  0
007200961010     D  WSVNRS                 8      9  0
007300961010     D  WSVNSP                10     16  0
007400961015     D  WSVSPE                 1     16
007500961010     D  WSVSTS                17     19
007600961010     D  WSVERR                20     22
007700960528     D WLBDA8          DS
007800960528     D  G02DAT                 1      8  0
007900960528     D  G02INV                 9     16  0
008000960528     D  G02ERR                17     17
008100960528     D  G02TGI                18     22  0
008200110630      *----------------------------------------------------*
008300110630      *  Invio E-mail
008400110630     D Indirizzi       s            253
008500110630     D Oggetto         s             44
008600110630     D Messaggio       s           5000
008700110630      *
008800110630     d   DSmail        ds
008900110630     D Mail_Ind                      44
009000110630     d   IND_char                     1    dim(44)
009100110630      *
009200110630     D Lunghezza_Indirizzo...
009300110630     D                 s              5u 0
009400110630     C*---------------------------------------------------------------*
009500110630     D MsgDSP          S            256                                         Testo generico
009600110630     C*---------------------------------------------------------------*
009700110630     D SndMg01         S             10                                         Message type
009800110630     D                                     INZ('*INFO')
009900110630     D SndMg02         S             10                                         Deluvery mode
010000110630     D                                     INZ('*BREAK')
010100110630     D SndMg03         S            256                                         Message text
010200110630     D SndMg04         S             10I 0                                      Length of text
010300110630     D                                     INZ(%SIZE(SndMg03))
010400110630     D SndMg05         S             10                                         User profile
010500110630     D                                     DIM(299)
010600110630     D SndMg06         S             10I 0                                      Number of user
010700110630     D SndMg07         S             10I 0                                      Message sent indic.
010800110630     D SndMg08         S             10I 0                                      Function requested
010900110630     D SndMg10         S              1                                         Show display
011000110630     D                                     INZ('N')
011100110630     D SndMg11         S             20                                         Qualified MSGQ name
011200110630     D SndMg12         S              4                                         Name type indicator
011300110630     D                                     INZ('*USR')
011400110630      * indice di scihera
011500110630     D Jx              S              5I 0
011600110630      *
011700110630     D/COPY QSYSINC/QRPGLESRC,QUSEC
011800110630      *
011900120905     d bart_it         C                   CONST('@Brt.it;')
012000120905     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
012100960708     D*
012200960528     I*---------------------------------------------------------------------*
012300960528     I* FILE
012400960528     I*---------------------------------------------------------------------*
012500960528     IIFTSTA00
012600960528     I              SSIFTSTA                    STADAT
012700960528     C*---------------------------------------------------------------------*
012800960528     C* Ciclo principale
012900960528     C*---------------------------------------------------------------------*
013000040708     C*
013100040708     C* resetta il flag x almeno una informazione supermercato
013200040708     c                   move      'N'           flg_almenouno     1
013300040707      *
013400960528      * Posizionamento iniziale sul file  (prima lettura)
013500110418     C                   open      EDSTA00F
013600110418     C                   read      EDSTA00F
013700110418     c                   exsr      EoF_EDSTA00F
013800961010      *
013900961010      * Loop letture fino a fine file EDSTA
014000110418     C                   DOW       not %eof(EDSTA00F)
014100040707     C                   MOVEL     STADAT        WDAT
014200040708      *
014300040707     C     STATPR        CASEQ     'ST00'        WRST00
014400040707     C     STATPR        CASEQ     'ST05'        WRST05
014500040707     C     STATPR        CASEQ     'ST10'        WRST10
014600040707     C     STATPR        CASEQ     'ST15'        WRST15
014700040708     C     STATPR        CASEQ     'SD00'        WRSD00
014800040708     C     STATPR        CASEQ     'SD05'        WRSD05
014900040708     C     STATPR        CASEQ     'SD10'        WRSD10
015000040708     C     STATPR        CASEQ     'SD15'        WRSD15
015100040708     C     STATPR        CASEQ     'SD20'        WRSD20
015200040708     C                   END
015300110330      *
015400110418     C                   read      EDSTA00F
015500110418     c                   exsr      EoF_EDSTA00F
015600961015     C                   END
015700110418     C                   close     EDSTA00F
015800040708      *
015900040708      * Se non ci sono righe da elaborare elimino tutto
016000040708     c                   if        flg_almenouno = 'N'
016100110418     C                   open      EDSTA00F
016200110418     C                   read      EDSTA00F
016300110418     C                   DOW       not %eof(EDSTA00F)
016400040708     C                   delete    IFTSTA00
016500110418     C                   read      EDSTA00F
016600040708     C                   END
016700040708     C                   END
016800960528     C* Fine pgm.
016900110418     C                   close     EDSTA00F
017000121010      **
017100121010      *   se si tratta di un traduttore NON EDIFACT deve far saltare al CL la parte
017200121010      *    successiva di trasmissione
017300121010     c                   if        §PZPGMTRSS = 'GELSTA'
017400131007     c                                or
017500131007     c                             §PZPGMTRSS = 'AMZSTA'
017600121010     c                   eval      esito = 'N'
017700121010     c                   end
017800121010      *
017900170711     c                   eval      %subst(KPJBU:241:1) = esito
018000960528     C                   SETON                                        LR
018100110330     C*----------------------------------------------------------------
018200110330     C*? x Fine File
018300110330     C*----------------------------------------------------------------
018400110418     C     EoF_EDSTA00F  BEGSR
018500110330      *
018600110330     C* se finito il File EoF
018700110418     C                   If          %eof(EDSTA00F)
018800110330     C                   MOVEL     'S'           WFINE             1
018900130821      *
019000130821      *  Solo se ci sono stati precedentemente dei records da scrivere
019100130821     c                   IF        flg_almenouno <> 'N'
019200110330      *
019300110330      *   e deve essere tradotto NON CON INTESA....
019400110330     c                   If        §PZPGMTRSS = 'STA94A'
019500110330     C                               and  Numero_Righe > 1
019600120831     c                               or
019700120831     c                             §PZPGMTRSS = 'GELSTA'
019800120831     C                               and  Numero_Righe > 0
019900131007     c                               or
020000131007     c                             §PZPGMTRSS = 'AMZSTA'
020100131007     C                               and  Numero_Righe > 0
020200120831      *
020300110330      *  Chiusura Messaggio UNT-UNZ se il traduttore non è INTESA
020400110330      *   quindi deve chiudere senza dare informazione di Righe di dettaglio
020500110819     C                   clear                   STADAT
020600110819     C                   clear                   wDAT
020700110819     c                   movel     'FINE'        STATPR
020800120831      *
020900120831     c                   exsr      write_OUTPUT
021000110330     c                   eval      Righe_Dett = 'N'
021100110413      *
021200110413     c                   else
021300110413      * deve uscire senza avere fatto nulla
021400110413     c                   eval      STATPR = 'RLBK'
021500120831     c                   exsr      write_OUTPUT
021600110330     c                   end
021700130821      *
021800130821     C                   EndIF
021900110330      *
022000110330     C                   EndIF
022100110330      *
022200110330     c                   EndSR
022300960528     C*----------------------------------------------------------------
022400040707     C*? WRST00 - Aggiorna da ATR a STA normali 781 diventa 784
022500960528     C*----------------------------------------------------------------
022600960528     C     WRST00        BEGSR
022700050902      *
022800050902     C* Caricamento NUMERATORE
022900050902     C                   EXSR      CARNUM
023000960528     C*  Pulisco dati DS
023100960528     C                   CLEAR                   EDST00
023200040707     C                   movel     STADAT        EDST00
023300960528     C*  Codice documento - nome documento
023400040707     C                   MOVEL     '784'         TA1001
023500960705     C                   MOVEL     EDST00        WDAT
023600120831      *  Testata messaggio  UNB-BGM-DTM
023700120831     c                   exsr      write_OUTPUT
023800960528     C*
023900960528     C                   ENDSR
024000960708     C*----------------------------------------------------------------
024100040707     C*? WRST05 - Nulla
024200960708     C*----------------------------------------------------------------
024300960708     C     WRST05        BEGSR
024400960708     C*
024500040708     C*  Pulisco dati DS
024600040708     C                   CLEAR                   EDST05
024700040708     C                   movel     STADAT        EDST05
024800040708     C                   MOVEL     EDST05        WDAT
024900120831      *  Testata messaggio  NAD-SF / NAD-ST
025000120831     c                   exsr      write_OUTPUT
025100040708     C*
025200960708     C                   ENDSR
025300960528     C*----------------------------------------------------------------
025400040707     C*? WRST10 - Nulla
025500960528     C*----------------------------------------------------------------
025600960528     C     WRST10        BEGSR
025700960528     C*
025800040708     C*  Pulisco dati DS
025900040708     C                   CLEAR                   EDST10
026000040708     C                   movel     STADAT        EDST10
026100040708     C                   MOVEL     EDST10        WDAT
026200120831      *  Testata messaggio  RFF-LOC
026300120831     c                   exsr      write_OUTPUT
026400040708     C*
026500960528     C                   ENDSR
026600960528     C*----------------------------------------------------------------
026700040707     C*? WRST15 - nulla
026800960528     C*----------------------------------------------------------------
026900960528     C     WRST15        BEGSR
027000960528     C*
027100040708     C*  Pulisco dati DS
027200040708     C                   CLEAR                   EDST15
027300040708     C                   movel     STADAT        EDST15
027400040708     C                   MOVEL     EDST15        WDAT
027500120831      *  Testata messaggio  FTX
027600120831     c                   exsr      write_OUTPUT
027700040708     C*
027800960528     C                   ENDSR
027900960528     C*----------------------------------------------------------------
028000040707     C*? WRSD00 - Aggancia la bolla x vedere se Supermercato
028100960528     C*----------------------------------------------------------------
028200960528     C     WRSD00        BEGSR
028300040708     C*
028400040708     C* resetta il flag x update o delete
028500040708     c                   move      'N'           flg_update        1
028600040708     C*
028700040708     C*  Pulisco dati DS
028800040708     C                   CLEAR                   EDSD00
028900040708     C                   MOVEL     STADAT        EDSD00
029000170711     c                   clear                   RIFER_Found       1
029100170707      **
029200170707      *  se non richiesta l'inversione del dato CNI nell'AGE
029300170707     c                   if        §ptTRD <> 'S'
029400170707      **
029500170707     C*  Imposto numero spedizione cliente
029600040708     C                   MOVEL     DA1004        CNI_WSV
029700040708     C                   MOVEL     '20'          wsvAAS
029800170707     **
029900170707     c                   else
030000170707     **
030100170707     **  trova la bolla per poter giudicare se il dettaglio è un Supermercato
030200170707     C                   move      'E'           tipar4
030300170707     C                   MOVEL     DA1004        notAr4
030400170707     C     Kar4          chain     fiar403l
030500170707     c                   if        %Found(fiar403l)
030600170707     **
030700170707     C                   z-add     ar4aas        WsvAAS
030800170707     C                   z-add     ar4lnp        WsvLNP
030900170707     C                   z-add     ar4nrs        WsvNRS
031000170707     C                   z-add     ar4nsp        WsvNSP
031100170711     c                   movel     'S'           RIFER_Found
031200170707     **
031300170707     c                   end
031400170707     c                   end
031500040708     **
031600170711     ** Solo se trovato il riferimento deve andare ad interrogare la BOLLA
031700170711     **  altrimenti deve saltare
031800170711     c                   if         RIFER_Found = 'S'
031900040708     ** aggancia la bolla x controllare se supermercato
032000040708     c     kblp          chain     FNblp01l
032100040708     c                   if        %Found(FnBLP01l) and
032200040721     c                             (blpTC1 = 'S' or blpTC2 = 'S' or
032300040721     c                              blpTC1 = 'A' or blpTC2 = 'A')
032400040708     c                   move      'S'           flg_update
032500040708     c                   end
032600170711     c                   endif
032700120831      *
032800040708     c                   if        flg_update = 'S'
032900120831     c                   move      'S'           flg_almenouno
033000040708     C                   ADD       1             WPROG
033100040708     C                   z-add     WPROG         DA1490
033200040708     C                   MOVEL     EDSD00        WDAT
033300120831      *  Testata messaggio  CNI-
033400120831     c                   exsr      write_OUTPUT
033500110330     C*
033600040708     c                   else
033700040708     C                   delete    IFTSTA00
033800040708     c                   end
033900960528     C*
034000960528     C                   ENDSR
034100960528     C*----------------------------------------------------------------
034200960528     C*? WRSD05 - Scrittura record partner - dettaglio
034300960528     C*----------------------------------------------------------------
034400960528     C     WRSD05        BEGSR
034500960528     C*
034600040708     C*  Pulisco dati DS
034700040708     C                   CLEAR                   EDSD05
034800040708     C                   MOVEL     STADAT        EDSD05
034900040708      *
035000040708      *  20+210 --> segnala se consegnata a un Supermercato
035100040708     C                   MOVEL     '20 '         DB9011
035200040708     C                   MOVEL     '210'         DB9013
035300120831      **
035400040708     c                   if        flg_update = 'S'
035500040708     C                   MOVEL     EDSD05        WDAT
035600120831      *  Testata messaggio  STS-RFF-DTM
035700120831     c                   exsr      write_OUTPUT
035800110330      *
035900110330     C                   MOVEL     'S'           WRTEVE            1
036000110330     c                   eval      Righe_Dett = 'S'
036100040708     c                   else
036200120831      *
036300040708     C                   delete    IFTSTA00
036400040708     c                   end
036500961010     C*
036600040708     C                   ENDSR
036700960528     C*-------------------------------------------------------------*
036800960528     C*? WRSD10 - Scrittura record partner - dettaglio
036900960528     C*----------------------------------------------------------------
037000960528     C     WRSD10        BEGSR
037100040708     C*
037200040708     C*  Pulisco dati DS
037300040708     C                   CLEAR                   EDSD10
037400040708     C                   MOVEL     STADAT        EDSD10
037500040708      *
037600040708     c                   if        flg_update = 'S'
037700040708     C                   MOVEL     EDSD10        WDAT
037800120831      *  Testata messaggio  NAD
037900120831     c                   exsr      write_OUTPUT
038000110330      *
038100040708     c                   else
038200040708     C                   delete    IFTSTA00
038300040708     c                   end
038400040708     C*
038500960528     C                   ENDSR
038600960528     C*-------------------------------------------------------------*
038700960528     C*? WRSD15 - Scrittura record partner - dettaglio
038800960528     C*----------------------------------------------------------------
038900960528     C     WRSD15        BEGSR
039000960528     C*
039100040708     C*  Pulisco dati DS
039200040708     C                   CLEAR                   EDSD15
039300040708     C                   MOVEL     STADAT        EDSD15
039400120831      *
039500040708     c                   if        flg_update = 'S'
039600040708     C                   MOVEL     EDSD15        WDAT
039700120831      *  Testata messaggio  FTX-
039800120831     c                   exsr      write_OUTPUT
039900110330     c                   eval      Righe_Dett = 'S'
040000120831      *
040100040708     c                   else
040200040708     C                   delete    IFTSTA00
040300040708     c                   end
040400040708     C*
040500960528     C                   ENDSR
040600960528     C*-------------------------------------------------------------*
040700960528     C*? WRSD20 - Scrittura record partner - dettaglio
040800960528     C*----------------------------------------------------------------
040900960528     C     WRSD20        BEGSR
041000960912     C*
041100040708     C*  Pulisco dati DS
041200040708     C                   CLEAR                   EDSD20
041300040708     C                   MOVEL     STADAT        EDSD20
041400120831     C*
041500040708     c                   if        flg_update = 'S'
041600040708     C                   MOVEL     EDSD20        WDAT
041700120831      *  Testata messaggio  GID-PCI
041800120831     c                   exsr      write_OUTPUT
041900110330     c                   eval      Righe_Dett = 'S'
042000110330     C*
042100040708     c                   else
042200040708     C                   delete    IFTSTA00
042300040708     c                   end
042400960912     C*
042500960912     C                   ENDSR
042600960730     C*----------------------------------------------------------------
042700960927     C*? CARNUM - CARICAMENTO NUMERATORE
042800960927     C*----------------------------------------------------------------
042900960927     C     CARNUM        BEGSR
043000960927     C*
043100960927     C                   Z-ADD     0             WPROG             4 0
043200960927     C                   Z-ADD     0             X                 3 0
043300960927     C                   Z-ADD     1             §MSNUM
043400961010     C                   MOVEL     'MS'          KCOD
043500961010     C                   MOVEL     *BLANKS       KKEY
043600040707     C                   MOVEL     '784'         KKEY
043700961010     C     KTAB          CHAIN     EDTAB01L                           30
043800960927     C     *IN30         IFEQ      '0'
043900960927     C     TABFLG        ANDEQ     *BLANKS
044000960927     C                   MOVEL     TABUNI        DSMS
044100960927     C                   ADD       1             §MSNUM
044200061219      *  il numero nel file è gestito max 4 caratteri quindi crea problemi se scatta
044300061219      *  ad esempio a 10000 o 20000 poichè il numeratore è + grande di 4 soli bytes.
044400061219      *  a questo punto incrementiamo in + la numerazione onde evitare il passaggio allo 0.
044500061219     c                   move      §MSNUM        campo_4n          4 0
044600061219     c                   if        campo_4n = 0
044700061219     c                   add       1             §MSNUM
044800061219     c                   end
044900960927     C                   Z-ADD     WOGGI         §MSDAT
045000960927     C                   MOVEL     DSMS          TABUNI
045100960927     C                   UPDATE    EDTAB000
045200960927     C                   END
045300960927     C*
045400960927     C                   ENDSR
045500960528     C*----------------------------------------------------------------
045600960528     C*? *INZSR - OPERAZIONI INIZIALI
045700960528     C*----------------------------------------------------------------
045800960528     C     *INZSR        BEGSR
045900960530     C*
046000960530     C     *ENTRY        PLIST
046100960530     C                   PARM                    KPJBA
046200170711      **** eliminato il secondo parametro
046300170711      * e spostato nella KPJBU alla posizione 241
046400170711      *  e serve solo in uscita
046500170711     C*******            PARM                    Esito             1            ritorno errore
046600170711      **
046700110330     C                   MOVEL     KPJBU         TRTC87
046800110601      *
046900110601      * reset campo compattazione x Download file
047000110630     C                   eval      esito = '0'
047100110601     c                   eval      Compatta_80 = 'S'
047200040708     C*
047300960528     C* Definisco chiavi di accesso
047400961010     C     KTAB          KLIST
047500961010     C                   KFLD                    KCOD
047600961010     C                   KFLD                    KKEY
047700040707     **
047800040708     C     Kblp          KLIST
047900040708     C                   KFLD                    WsvAAS
048000040708     C                   KFLD                    WsvLNP
048100040708     C                   KFLD                    WsvNRS
048200040708     C                   KFLD                    WsvNSP
048300170707      *
048400170707     C     Kar4          KLIST
048500170707     C                   KFLD                    tipar4            1
048600170707     C                   KFLD                    notar4           30
048700170707     **
048800120831      * DEVE ESSERE PILOTATO DALLA TAB.3K
048900120831     C     KTAB2         KLIST
049000120831     C                   KFLD                    TBLKUT
049100120831     C                   KFLD                    TBLCOD
049200120831     C                   KFLD                    TBLKEY
049300040708     **
049400980330     C     *LIKE         DEFINE    TABCOD        KCOD
049500961010     C     *LIKE         DEFINE    TABKEY        KKEY
049600960528     C*
049700960528     C* Recupero codice AS
049800960528     C                   Z-ADD     1             CODUT             1 0
049900960705     C                   CALL      'X§PARUT'
050000960528     C                   PARM                    UTEDSE
050100960528     C                   MOVEL     REC80         CNCR80
050200980622     C                   MOVEL     '005'         WAS400            3
050300040708     C*
050400960528     C* Recupero data e ora x messaggio
050500960528     C                   TIME                    WHHDAT           14 0
050600960708     C                   MOVE      WHHDAT        G02DAT
050700960528     C                   MOVE      WHHDAT        DATA              6 0
050800960528     C                   MOVEL     *BLANK        G02ERR
050900960528     C                   CALL      'XSRDA8'
051000960528     C                   PARM                    WLBDA8
051100960708     C                   Z-ADD     G02INV        WOGGI             8 0           UDATE
051200960708     C                   MOVEL     WHHDAT        WHHMM             4 0
051300960528     C                   MOVE      WOGGI         WOGGI6            6 0
051400960528     C                   MOVE      WHHDAT        WANNO             2 0
051500960528     C                   MOVEL     WANNO         WOGGI6
051600040708     C*
051700960528     C* Caricamento Tabella Partner esteri
051800960528     C                   Z-ADD     0             X                 3 0
051900110330     C                   MOVEL     'PT'          KCOD
052000110330     C                   MOVEL     D87CPT        KKEY
052100120831     C                   clear                   DSPT
052200110330     C     KTAB          CHAIN     EDTAB01L                           30
052300110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
052400110330     C                   MOVEL     TABUNI        DSPT
052500110330     C                   END
052600110330     C                   MOVEL     'PU'          KCOD
052700120831     C                   clear                   DSPU
052800110330     C     KTAB          CHAIN     EDTAB01L                           30
052900110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
053000110330     C                   MOVEL     TABUNI        DSPU
053100110330     C                   END
053200110330      *   Con quale traduttore deve essere scaricato e scritto il File ????
053300110330     C                   MOVEL     'PZ'          KCOD
053400120831     C                   clear                   DSPZ
053500110330     C     KTAB          CHAIN     EDTAB01L
053600110330     C                   if        %Found(EDTAB01L) and tabflg = *blank
053700110330     C                   MOVEL     TABUNI        DSPZ
053800110330     C                   END
053900120831      *
054000120831     c                   eval      CLIENTE =  '0' + %editc(§PTKSC:'X')
054100120831     C                   Z-ADD     1             TBLKUT
054200120831     C                   MOVEL     '3K'          TBLCOD
054300120831     C                   moveL(P)  §PTKSC        TBLKEY
054400120831     C     KTAB2         chain     tabel00f
054500120831     c                   if        %Found(tabel00f)
054600120831     c                   eval      ds3K = tblUNI
054700120831     c                   eval      CLIENTE =  '0' + %editc(§3Kcks:'X')
054800120831     C                   END
054900110330      *>>>>>>>>>>
055000960528     C                   ENDSR
055100120831      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
055200120831      *  Scrive l'OUTPUT da inviare
055300120831      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
055400120831     C     write_OUTPUT  BEGSR
055500120831      *
055600120831     C                   Z-ADD     §MSNUM        STAPRG
055700120831     C                   MOVEL     WDAT          STADAT
055800120831      *
055900120831     C*  aggiorno IFTSTA
056000120831      * Scrittura file per 20/210 Appuntamento da ATR
056100120831     c                   if        STATPR <> 'FINE' and
056200120831     c                             STATPR <> 'RLBK'
056300120831     C                   update    IFTSTA00
056400120831     c                   end
056500120831      *
056600120831     c                   If        §PZPGMTRSS = 'STA94A'
056700120831     c                   exsr      Exit_segmenti
056800120831     c                   elseIf    §PZPGMTRSS = 'GELSTA'
056900120831     c                   exsr      Exit_FileGEL
057000131007     c                   elseIf    §PZPGMTRSS = 'AMZSTA'
057100131007     c                   exsr      Exit_FileAMZ
057200120831     C                   EnD
057300120831      *
057400120831     C                   ENDSR
057500110330      *'''''''''''''''''''''''''''''''''''''''''''''''''''''''
057600110330      *  Scrittura dei segmenti IFTSTA D94A
057700110330      *,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
057800110330     C     Exit_Segmenti BEGSR
057900110330      *
058000110330     c                   eval      Tipo_DS = STATPR
058100110330     c                   eval      Suffisso   = §PUSIG
058200110601     c                   eval      Compatta_80 = §pucompat
058300110330     c                   clear                   DsGenerica
058400110330     c                   eval      DsGenerica = STADAT
058500110330     c                   call      'TRTCT81R1'
058600110330     c                   PARM                    CLIENTE           8
058700110330     c                   PARM                    Suffisso          2
058800110330     c                   PARM                    Tipo_DS           4
058900110330     c                   PARM                    DsGenerica     1024
059000110330     C                   PARM                    Numero_Righe      5 0
059100110601     c                   PARM                    Compatta_80       1
059200110617     C                   PARM                    Progressivo      10
059300110621     c                   PARM                    Esito             1
059400110630     C
059500110630     C                   if        esito = 'E'
059600110630     c                   exsr      Invio_Mail
059700110630     c                   end
059800110330      *
059900110330     C                   ENDSR
060000120831     c*==================================================================*
060100120831      * Manda un File a tracciato NETEXPRESS EUROPE
060200120831     c*==================================================================*
060300120831     c     Exit_FileGEL  begsr
060400120831      *
060500120831     c                   clear                   DsGenerica
060600120831     c                   eval      Tipo_DS = STATPR
060700120831     c                   eval      DsGenerica = STADAT
060800120905     c                   call      'TRTCT81GR'
060900120831     c                   PARM                    CLIENTE
061000120831     c                   PARM                    Tipo_DS           4
061100120831     c                   PARM                    DsGenerica     1024
061200120831     C                   PARM                    Numero_Righe      5 0
061300120831     C                   PARM                    Progressivo      10
061400120831     c                   PARM                    esito             1
061500120831     C
061600120831     C                   if        esito = 'E'
061700120831     c                   exsr      Invio_Mail
061800120831     c                   end
061900120831      *
062000120831     C                   ENDSR
062100131007     c*==================================================================*
062200131007      * Manda un File a tracciato IFTSTA Particolarissimo di AMAZON
062300131007     c*==================================================================*
062400131007     c     Exit_FileAMZ  begsr
062500131007      *
062600131007     c                   clear                   DsGenerica
062700131007     c                   eval      Tipo_DS = STATPR
062800131007     c                   eval      DsGenerica = STADAT
062900131007     c                   call      'TRTCT81AR'
063000131007     c                   PARM                    CLIENTE
063100131007     c                   PARM                    Tipo_DS           4
063200131007     c                   PARM                    DsGenerica     1024
063300131007     C                   PARM                    Numero_Righe      5 0
063400131007     C                   PARM                    Progressivo      10
063500131007     c                   PARM                    esito             1
063600131007     C
063700131007     C                   if        esito = 'E'
063800131007     c                   exsr      Invio_Mail
063900131007     c                   end
064000131007      *
064100131007     C                   ENDSR
064200110630     c*==================================================================*
064300110630      * Manda un Msg x E-mail
064400110630     c*==================================================================*
064500110630     c     Invio_Mail    begsr
064600110630      *
064700120831     c                   clear                   CLIENTEksc        8
064800120831     c                   eval      CLIENTEksc =  '0' + %editc(§PTKSC:'X')
064900110630      *
065000130821???? C                   EVAL      Oggetto ='TRTCT89R:DOWNLD ATR STATI EDI !!'
065100110630     C                   EVAL      Messaggio = 'ATTENZIONE : traduzione ATR EDI-
065200110630     C                              STATI NON tradotto x invio su DOWNLD. -
065300110630     C                             Controllare il Download del Cliente. << '+
065400170623     c                             CLIENTEksc + ' >>'  + da_dove
065500110630      *
065600110630     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
065700110630     C* Inizializzo variabili
065800110630     C                   movel     *blanks       wrkEml          253
065900110630     C                   movel     *blanks       wrkMsg         5000
066000110630     C                   movel     *blanks       wrkOgg           44
066100110630     C* Valorizzo i campi della e-m@ail - indirizzo
066200110630     C                   eval      wrkEml = CED_BART
066300110630     C                   eval      wrkOgg = Oggetto
066400110630     C                   eval      wrkMsg = Messaggio
066500110630     C                   call(e)   'TRTCT00R2'
066600110630     C                   parm                    wrkEml
066700110630     C                   parm                    wrkOgg
066800110630     C                   parm                    wrkMsg
066900110630     C*
067000110630     C                   ENDSR
067100110630      * ?------------------------------------------------------------------ */
067200110630      *?      X non bloccare in nessun caso il traduttore CLIENTI
067300110630      * ?------------------------------------------------------------------ */
067400110630     C     *pssr         BEGSR
067500110630     C
067600110630     C                   eval      esito = 'E'
067700170623     C                   movel     'PSSR'        da_dove           4
067800110630     c                   exsr      Invio_Mail
067900170711     c                   eval      %subst(KPJBU:241:1) = esito
068000110630     C
068100110630     C                   ENDSR     '*CANCL'
068200110630      *  ------------------------------------------------------------------ */
