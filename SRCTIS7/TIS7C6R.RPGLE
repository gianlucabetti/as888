000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300000907     H* Gestione codici unificanti
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900020211     Ftis7c6P   O    E             PRINTER OFLIND(*IN05)
001000020212     Ftivss02l  IF   E           K DISK
001100020211     Ftikun01l  IF   E           K DISK
001200020211     Ftikun03L  IF   E           K DISK
001300020212     F                                     RENAME(tikun000:tikun003)
001400020211     Ftikun06L  IF   E           K DISK
001500020211     F                                     RENAME(tikun000:tikun006)
001600020212     Ftntbe01l  IF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000991214     D*------------
002100970526     D* ARCHITETTURA
002200970526     D*------------
002300970526     D KPJBA         E DS
002400991214     D*------------------
002500991214     D* DS X PASSAGGIO PARAMETRI
002600991214     D*------------------
002700991214     D PARAM           DS                  INZ
002800020211     D  PARATT                 1      1
002900020211     D  PARDAT                 2      9  0
003000020211     D  PARDAI                10     17  0
003100020710     D  PARTLE                18     19
003200020208     D*-------------------
003300020208     D* DS REPERIMENTO PADRE/FIGLI
003400020208     D*-------------------
003500020208     D bs69ds        E DS                  EXTNAME(tibs69ds) INZ
003600020208     D acods         E DS                  EXTNAME(cnaco00f) INZ
003700020208     D indds         E DS                  EXTNAME(cnind00f) INZ
003800020208     D clpds         E DS                  EXTNAME(cnclp00f) INZ
003900020208     D clsds         E DS                  EXTNAME(fncls00f) INZ
004000020212     D*------------
004100020212     D* DS X LETTURA NUOVA TABELLA ISV
004200020212     D*------------
004300020212     D DSISV         E DS                  EXTNAME(DISV)
004400011206     D*-------------------
004500011206     D* VARIABILI DI WRK
004600011206     D*-------------------
004700020211     D depksu                              like(vssksu)
004800020211     D depisv                              like(vssisv)
004900020211     D deptle                              like(vsstle)
005000020212     D isvkun          s              2    dim(100)
005100970526     C*------------------------------------------------------------------------*
005200991214     C* MAIN LINES
005300970526     C*------------------------------------------------------------------------*
005400970526     C*
005500991214     C* ESEGUE ROUTINE DI CREAZIONE E STAMPA
005600991214     C                   EXSR      ESEGUI
005700020208     C                   EXSR      FINSR
005800000306     C*
005900970526     C                   SETON                                        LR
006000970526     C*------------------------------------------------------------------------*
006100991217     C* ESEGUI - OPERAZIONI PRINCIPALI
006200970526     C*------------------------------------------------------------------------*
006300991214     C     ESEGUI        BEGSR
006400991214     C*
006500011205     C* Per prima cosa valorizzo i parametri di lancio in stampa
006600011205     C                   exsr      valPAR
006700011205     C*
006800011205     C* Poi stampo l'intestazione
006900011205     C                   seton                                        05
007000011205     C                   exsr      stpTES
007100020207     C*
007200020211     C* Quindi eseguo l'elaborazione
007300020211     C                   exsr      exeVSS
007400011205     C*
007500011205     C* Al termine chiudo il prospetto
007600020211     C                   write     s7c6RI
007700020211     C                   write     s7c6FI
007800970526     C*
007900970526     C                   ENDSR
008000011205     C*------------------------------------------------------------------------*
008100011205     C* VALPAR - VALORIZZA I PARAMETRI DI LANCIO IN STAMPA
008200011205     C*------------------------------------------------------------------------*
008300011205     C     VALPAR        BEGSR
008400011205     C*
008500020710     C                   if        %trim(PARTLE) = '*'
008600020710     C                   eval      STPNOTE = 'Tutti i legami '
008700020710     C                   else
008800020710     C                   eval      STPNOTE = 'Solo unificazioni di tipo '+
008900020710     C                                       PARTLE
009000020710     C                   endif
009100020710     C                   if        PARATT  = 'S'
009200020710     C                   eval      STPNOTE = %trim(STPNOTE)+' e solo clienti '+
009300020710     C                                       'con abilitazioni attive'
009400020710     C                   endif
009500020211     C                   eval      STPDAT  = PARDAI
009600011205     C*
009700011205     C                   ENDSR
009800011205     C*------------------------------------------------------------------------*
009900011205     C* STPTES - ESEGUE LA STAMPA DELLA TESTATA QUANDO NECESSARIO
010000011205     C*------------------------------------------------------------------------*
010100011205     C     STPTES        BEGSR
010200011205     C*
010300011205     C                   if        *in05 = *on
010400020211     C                   write     s7c6TE
010500011205     C                   setoff                                       05
010600011205     C                   endif
010700011205     C*
010800011205     C                   ENDSR
010900000929     C*------------------------------------------------------------------------*
011000020211     C* EXEVSS - Routine di verifica record file abilitazioni (TIVSS00F)
011100000929     C*------------------------------------------------------------------------*
011200020211     C     EXEVSS        BEGSR
011300020710     C*
011400020710     C                   movel     'S'           wrecok            1
011500011205     C*
011600020211     C* Leggo tutto il file delle abilitazioni
011700020211     C     *loval        setll     tivss02l
011800020211     C                   read      tivss02l
011900020211     C                   dow       not %eof(tivss02l)
012000020212     C* Verifico che il servizio del record cotrrente sia soggetto a controllo unificazioni
012100020212     C                   if        vssvat = *blanks                             *no annull/sospeso
012200020710     C* Se nel lancio richiesto un tipo legame specifico => considero solo quelli
012300020710     C                   if        %trim(PARTLE) <> '*'
012400020710     C                   eval      kkutle = PARTLE
012500020710     C                   eval      kkuazp = *blanks
012600020710     C                   move      vssksu        kkucop
012700020710     C     keykun        chain     tikun01l
012800020710     C                   if        %found(tikun01l)
012900020710     C* Se esiste il legame verifico che sia ancora valido
013000020710     C                   if        PARATT = 'S'     and
013100020710     C                             (PARDAT < kundde or                           *scaduti
013200020710     C                              PARDAT > kundsc)
013300020710     C                   movel     'N'           wrecok
013400020710     C                   else
013500020710     C                   movel     'S'           wrecok
013600020710     C                   endif
013700020710     C                   else
013800020710     C* Se l'abilitazione presenta un legame del tipo richiesto => considero in ogni caso
013900020710     C                   if        vsstle = PARTLE or
014000020710     C                             %trim(PARTLE) = '*'
014100020710     C                   movel     'S'           wrecok
014200020710     C                   else
014300020710     C                   movel     'N'           wrecok
014400020710     C                   endif
014500020710     C                   endif
014600020710     C                   endif
014700020212     C* Se nel lancio richiesto solo clienti attivi => verifico
014800020710     C                   if        wrecok = 'S'
014900020212     C                   if        PARATT = 'S'     and
015000020212     C                             (PARDAT < vssdde or                          *scaduti
015100020212     C                              PARDAT > vssdsc)
015200020212     C                   else
015300020212     C     vssisv        lookup    isvkun                                 22    *attivi
015400020212     C                   if        *in22 = *on
015500020211     C                   exsr      exePRT
015600020211     C                   endif
015700020211     C                   endif
015800020212     C                   endif
015900020710     C                   endif
016000020211     C                   read      tivss02l
016100020211     C                   enddo
016200000929     C*
016300000929     C                   ENDSR
016400011205     C*------------------------------------------------------------------------*
016500020211     C* EXEPRT - STAMPA IL DETTAGLIO
016600011205     C*------------------------------------------------------------------------*
016700020208     C     EXEPRT        BEGSR
016800020211     C*
016900020211     C                   clear                   s7c6de
017000011205     C                   exsr      repinfo
017100020208     C                   exsr      repmess
017200011206     C*
017300011205     C                   ENDSR
017400020211     C*------------------------------------------------------------------------*
017500020211     C* EXEPRTDET - STAMPA LA RIGA PREVIE CONSIDERAZIONI
017600020211     C*------------------------------------------------------------------------*
017700020211     C     EXEPRTDET     BEGSR
017800020211     C*
017900020211     C* Stampo solo se presenti errori
018000020211     C                   exsr      stpTES
018100020211     C                   if        stpmess <> *blanks
018200020211     C                   if        stpksu = depksu
018300020211     C                   eval      stpksu = *blanks
018400020211     C                   eval      stprag = *blanks
018500020212     C                   eval      depksu = stpksu
018600020211     C                   endif
018700020211     C                   if        stpisv = depisv
018800020211     C                   eval      stpisv = *blanks
018900020212     C                   eval      depisv = stpisv
019000020211     C                   endif
019100020211     C                   if        stptle = deptle
019200020211     C                   eval      stptle = *blanks
019300020212     C                   eval      deptle = stptle
019400020211     C                   endif
019500020211     C                   write     s7c6DE
019600020211     C                   endif
019700020211     C                   clear                   s7c6DE
019800020211     C*
019900020211     C                   ENDSR
020000011205     C*------------------------------------------------------------------------*
020100020208     C* REPINFO - REPERISCE TUTTE LE INFORMAZIONI NECESSARIE
020200011205     C*------------------------------------------------------------------------*
020300011205     C     REPINFO       BEGSR
020400020208     C*
020500020211     C* Valorizzo i dati del cliente abilitato
020600020211     C                   eval      stpisv = vssisv
020700020211     C                   eval      stpksu = vssksu
020800020211     C                   eval      stptle = vsstle
020900020208     C*
021000020208     C* Reperisco le informazioni anagrafiche
021100020208     C                   clear                   bs69ds
021200020208     C                   clear                   acods
021300020208     C                   clear                   indds
021400020208     C                   clear                   clpds
021500020208     C                   clear                   clsds
021600020208     C                   movel     knsif         i69sif
021700020211     C                   move      vssksu        I69KAC
021800020208     C                   call      'TIBS69R'
021900020208     C                   parm                    bs69ds
022000020208     C                   parm                    acods
022100020208     C                   parm                    indds
022200020208     C                   parm                    clpds
022300020208     C                   parm                    clsds
022400020208     C*
022500020208     C     o69err        ifne      '1'
022600020211     C                   eval      stprag = acorag
022700020208     C                   endif
022800011206     C*
022900011205     C                   ENDSR
023000020208     C*------------------------------------------------------------------------*
023100020211     C* REPMESS - EFFETTUA CONSIDERAZIONI E DETERMINA IL MESAGGIO DA ESPORRE
023200020208     C*------------------------------------------------------------------------*
023300020208     C     REPMESS       BEGSR
023400020211     C*
023500020211     C                   movel     'N'           flgKun            1
023600020710     C*
023700020710     C* X sicurezza mi riposiziono all'inizio del file tikun01l
023800020710     C     *loval        setll     tikun01l
023900020710     C*
024000020710     C* Clienti figli collegati
024100020211     C     vsstle        ifne      *blanks                                      *c'è legame
024200020211     C                   movel     vsstle        kkutle
024300020211     C                   movel     *blanks       kkuazp
024400020211     C                   move      vssksu        kkucop
024500020211     C     keykun        setll     tikun01l
024600020211     C     keykun        reade     tikun01l                               98
024700020211     C                   dow       not *in98
024800020211     C     kunvat        ifeq      *blanks
024900020710     C     kundsc        iflt      PARDAT                                       *non valido
025000020710     C     kundde        orgt      PARDAT
025100020211     C                   else                                                   *valido
025200020211     C                   movel     'S'           flgKun
025300020211     C                   leave
025400020211     C                   endif
025500020211     C                   endif
025600020211     C     keykun        reade     tikun01l                               98
025700020211     C                   enddo
025800020211     C                   if        flgKun = 'N'
025900020211     C                   eval      stpmess = 'ATTENZIONE: non esistono ' +
026000020211     C                                       'figli legati.'
026100020211     C                   exsr      EXEPRTDET
026200020211     C                   endif
026300020211     C*
026400020211     C                   else
026500020211     C*
026600020211     C                   move      vssksu        kkucop
026700020211     C     ke3kun        setll     tikun03l
026800020211     C     ke3kun        reade     tikun03l                               98
026900020211     C                   dow       NOT *in98
027000020211     C     kunvat        ifeq      *blanks
027100020211     C     kuntle        andne     vsstle
027200020710     C     kundsc        iflt      PARDAT                                       *non valido
027300020710     C     kundde        orgt      PARDAT
027400020211     C                   else
027500020211     C                   movel     'S'           flgKun                         *esistono kun
027600020211     C                   leave
027700020211     C                   endif
027800020211     C                   endif
027900020211     C     keykun        reade     tikun01l                               98
028000020211     C                   enddo
028100020211     C                   if        flgKun = 'S'
028200020211     C                   eval      stpmess = 'ATTENZIONE: esistono ' +
028300020211     C                                       'figli legati.'
028400020211     C                   exsr      EXEPRTDET
028500020211     C                   endif
028600020211     C                   endif
028700020212     C*
028800020212     C* Che nn sia cliente figlio
028900020212     C                   move(p)   vssksu        kkucof
029000020710     C                   if        %trim(PARTLE) <> '*'
029100020710     C                   movel     PARTLE        kkutle
029200020710     C                   else
029300020710     C                   movel     *blanks       kkutle
029400020710     C                   endif
029500020212     C     ke6kun        setll     tikun06l
029600020212     C     ke6kun        reade     tikun06l
029700020212     C                   dow       not %eof(tikun06l)
029800020212     C     kunvat        ifeq      *blanks                                      *no annull/sospesi
029900020710     C                   if        kundsc <  PARDAT  or                         *periodo esterno
030000020710     C                             kundde >  PARDAT
030100020212     C                   else                                                   *periodi sovrapp.
030200020212     C                   eval      stpmess = 'ATTENZIONE: cliente abilitato ' +
030300020212     C                                       'risulta essere cliente figlio.'
030400020212     C                   exsr      EXEPRTDET
030500020212     C                   leave
030600020212     C                   endif
030700020212     C                   endif
030800020212     C     ke6kun        reade     tikun06l
030900020212     C                   enddo
031000020208     C*
031100020208     C                   ENDSR
031200020208     C*------------------------------------------------------------------------*
031300020208     C* FINSR - OPERAZIONI FINALI
031400020208     C*------------------------------------------------------------------------*
031500020208     C     FINSR         BEGSR
031600020208     C*
031700020208     C* CHIUDE I FILE DEI PGM CHIAMATI
031800020208     C                   CLEAR                   bs69ds
031900020208     C                   CLEAR                   acods
032000020208     C                   CLEAR                   indds
032100020208     C                   CLEAR                   clpds
032200020208     C                   CLEAR                   clsds
032300020208     C                   MOVEL     'C'           i69tla
032400020208     C                   MOVEL     knsif         i69sif
032500020208     C                   CALL      'TIBS69R'
032600020208     C                   PARM                    bs69ds
032700020208     C                   PARM                    acods
032800020208     C                   PARM                    indds
032900020208     C                   PARM                    clpds
033000020208     C                   PARM                    clsds
033100020208     C*
033200020208     C                   ENDSR
033300020212     C*--------------------------------------------------------------------------------------------*
033400020212     C* CARTAB - carica le tabelle occorrenti
033500020212     C*--------------------------------------------------------------------------------------------*
033600020212     C     CARTAB        BEGSR
033700020212     C*----------
033800020212     C* Tipi servizi soggetti a controllo delle abilitazioni
033900020212     C*----------
034000020212     C                   z-add     *zeros        x                 3 0
034100020212     C                   eval      tbecod = 'ISV'
034200020212     C     keytbe        setll     tntbe01l
034300020212     C     keytbe        reade     tntbe01l
034400020212     C                   dow       not %eof(tntbe01l)
034500020212     C                   movel     tbeuni        dsisv
034600020212     C                   if        §isvuni  = 'S'
034700020212     C                   add       1             x
034800020212     C                   eval      isvkun(x) = tbeke1
034900020212     C                   endif
035000020212     C     keytbe        reade     tntbe01l
035100020212     C                   enddo
035200020212     C*
035300020212     C                   ENDSR
035400970526     C*------------------------------------------------------------------------*
035500970526     C* *INZSR - OPERAZIONI INIZIALI
035600970526     C*------------------------------------------------------------------------*
035700970526     C     *INZSR        BEGSR
035800970526     C*---
035900970526     C* RICEVIMENTO PARAMETRI
036000970526     C*---
036100970526     C     *ENTRY        PLIST
036200970612     C                   PARM                    KPJBA
036300991214     C                   MOVEL     KPJBU         PARAM
036400020212     C*
036500020212     C* CARICA LE TABELLE OCCORRENTI
036600020212     C                   EXSR      CARTAB
036700020211     C*---
036800020211     C* VARIABILI RIFERITE AL DATA BASE
036900020211     C*---
037000020211     C     *LIKE         DEFINE    kuntle        kkutle
037100020211     C     *LIKE         DEFINE    kunazp        kkuazp
037200020211     C     *LIKE         DEFINE    kuncop        kkucop
037300020212     C     *LIKE         DEFINE    kuncof        kkucof
037400020212     C     *LIKE         DEFINE    kundde        kkudde
037500970526     C*---
037600991214     C* CHIAVI DI LETTURA
037700991214     C*---
037800020211     C* LETTURA tikun01L -
037900020211     C     KEYkun        KLIST
038000020211     C                   KFLD                    kkutle                         *TERMINAL PARTENZA
038100020211     C                   KFLD                    kkuazp                         *PARTENZA/ARRIVO
038200020211     C                   KFLD                    kkucop                         *PARTENZA/ARRIVO
038300020212     C* LETTURA tikun03L
038400020211     C     KE3kun        KLIST                                                  *tikun03l
038500020211     C                   KFLD                    kkucop                          -cliente padre
038600020211     C* LETTURA tikun06L
038700020211     C     KE6kun        KLIST
038800020212     C                   KFLD                    kkucof                         *CODICE CLI FIGLIO
038900020212     C                   KFLD                    kkutle                         *TIPO LEGAME
039000020212     C* LETTURA tntbe01l - PARZIALE
039100020212     C     KEYtbe        KLIST
039200020212     C                   KFLD                    tbecod                         *CODICE TABELLA
039300991214     C*
039400970526     C                   ENDSR
