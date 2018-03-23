000100950825     H DECEDIT('0,') DATEDIT(*DMY.)
000200950825     H* TRMK45R *-----------------------------------------------------*
000300950825     H*       CAMBIO CODICE COMMERCIALE (BATCH)                       *
000400950825     H*---------------------------------------------------------------*
000500981204     FAZORG01L  IF   E           K DISK
000600041110     FTNCPO08L  iF   E           K DISK    infds(cpoinf)
000700041110     fTncpo00f  uf   e             disk    rename(tncpo000:tncpo00)
000800101115     FTNCPO01L  IF   E           K DISK    RENAME(TNCPO000:TNCPO001)
000900050201     FCNCLP01L  iF   E           K DISK    infds(clpinf)
001000050201     fCnclp00f  uf   e             disk    rename(Cnclp000:cnclp00)
001100950828     FCNACO00F  UF   E           K DISK
001200050927     FTFCLP01l  UF   E           K DISK    RENAME(CNCLP000:TFCLP000)
001300950830     FTFSAC01L  UF   E           K DISK
001400101115
001500101115     fTIATC00F  UF   E             DISK
001600101115     fTIVIS00F  UF   E             DISK
001700101115     fTIVIS05L  iF   E           K DISK    rename(TIVIS000:TIVIS05)
001800101115
001900100519     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002000041110
002100041110     d wbatch          s              1    inz('0')
002200101115
002300101115     d $EndF1          s               n   inz(*off)
002400101115     d $EndF2          s               n   inz(*off)
002500101115     d $EndF3          s               n   inz(*off)
002600101115
002700101115     d w_ATCrcd        s             15  0 inz
002800101115     d w_VISrcd        s             15  0 inz
002900101115
003000101115     d testata         s              1    inz
003100041110
003200950825     D KPJBA         E DS
003300950825     D PARAM           DS
003400950825     D  PACCMV                 1      7
003500950825     D  PACCMN                 8     14
003600950830     D SACDS         E DS                  EXTNAME(TFSAC01L)
003700950830     D SACMEM          DS
003800950830     D  MCCMM                  1      4P 0
003900981029     D  MCAAA                  5      7P 0
004000981029     D  CCMMM                  8      9P 0
004100981029     D  CCAPL                 10     10
004200981029     D  CCTPV                 11     11
004300981029     D  CCCLV                 12     12
004400981029     D  CCFIE                 13     13
004500981029     D  CCFFZ                 14     14
004600981029     D  MCTO1                 15     18P 0
004700981029     D  MCTO2                 19     22P 0
004800981029     D  MCTO3                 23     26P 0
004900981029     D  MCTO4                 27     30P 0
005000971121     D                 DS
005100960906     D  CLPKSC                 1      7  0
005200960906     D  STRKSC                 1      5  0
005300020902      *-----------------------------------------------------------
005400020902      * Ds di riferimento al file esterno AzUte00F
005500020902      *--------
005600020902     D AzuteDs       E DS                  ExtName(AzUte00F)
005700020902      *-----------------------------------------------------------
005800020902      * Ds per dati organigramma
005900020902      *--------
006000020902     D DDatiUte      E DS
006100020902      *-----------------------------------------------------------
006200020902      * Parametri per richiamo al pgm di controllo profilo utenti
006300020902      *--------
006400020902     D Tibs34Ds      E DS                  Inz
006500041110      * DS Esterna x gestione allocazione ed invio messaggi
006600041110     d Trul82ds      e ds
006700100519     d TNTA43DS      e ds
006800101115
006900101115      // File attività
007000101115     d tiatcds       e ds                  extname(TIATC00F)
007100101115     d                                     prefix(w_)
007200101115
007300101115      // File trattative
007400101115     d tivisds       e ds                  extname(TIVIS00F)
007500101115     d                                     prefix(w_)
007600160804
007700160804       // -?Storicizzazione variazioni
007800160804     d TRMK30DS      e ds
007900160804     d TNCPO_30      e ds                  extname(TNCPO00F) inz
008000160804     d TNCPO1_30     e ds                  extname(TNCPO10F) inz
008100160804     d TICPI_30      e ds                  extname(TICPI00F) inz
008200041110
008300041110     d cpoinf          ds
008400041110     d  cponrr               397    400i 0
008500050201
008600050201     d clpinf          ds
008700050201     d  clpnrr               397    400i 0
008800040806
008900040806     D WLBDA8          DS
009000040806     D  G08DAT                 1      8  0
009100040806     D  G08INV                 9     16  0
009200040806     D  G08ERR                17     17
009300040806     D  G08TGI                18     22  0
009400041110
009500041110     d msgjob          C                   CONST('Si sta bloccando l''aggior.de-
009600041110     d                                     i codici commerciali:USCIRE SUBITO d-
009700041110     d                                     a questo lavoro')
009800101115
009900101115      //---------------------------------------------------------------
010000101115      //?Definizione procedure esterne.
010100101115      //---------------------------------------------------------------
010200101115     d trul82r         pr                  extpgm('TRUL82R')
010300101115     d  trul82ds                           likeds(TRUL82DS)
010400101115
010500101115     D Tnta43R         pr                  extpgm('TNTA43R')
010600101115     D  tnta43ds                           likeds(tnta43ds)
010700160804
010800160804       // -?Storicizzazione Variazioni Potenziale
010900160804     d TRMK30R         pr                  extpgm('TRMK30R')
011000160804     d  trmk30ds                           likeds(trmk30ds)
011100160804     d  tncpo_30                           likeds(tncpo_30)
011200160804     d  tncpo1_30                          likeds(tncpo1_30)
011300160804     d  ticpi_30                           likeds(ticpi_30)
011400101115
011500950831     C****************************************************************
011600950831     C*  RIEPILOGO INDICATORI
011700950831     C***************************************************************
011800950831     C* 20    - PER QCMDEXC
011900950831     C* 30    - COMODO
012000950829     C******
012100981201     C* CAMBIO CODICE COMMERCIALE SU TNCPO
012200981204     C*  NON HO BISOGNO DI TESTARE SE E' DEL MIO SISTEMA
012300981204     C*  PERCHE' IL COD COMMERCIALE DEVE ESSERE DELL'AREA PER GESTIRLO
012400950829     C******
012500981201     C     CCMV          SETLL     TNCPO08L
012600981201     C     CCMV          READE     TNCPO08L                               30
012700950828     C     *IN30         DOWEQ     '0'
012800950828     C     CPOATB        IFNE      'A'
012900041110
013000041110      * Gestisco allocazione del record con invio del msg all'utente che blocca il rcd
013100041110     c     cponrr        Chain(e)  Tncpo00f
013200041110    2c                   If        %Error
013300041110     c                   Clear                   Trul82ds
013400041110     c                   Eval      ul82§rrn = cponrr
013500041110     c                   Eval      UL82§FIL = 'TNCPO00F'
013600041110     c                   Eval      UL82§WIN = 'N'
013700041110     c                   Eval      UL82§F7  = 'S'
013800041110     c                   Eval      UL82§num = 5
013900041110     c                   Eval      UL82§att = 60
014000041110     c                   Eval      UL82§mss = msgjob
014100041110      * Effettuo la chiamata al *PGM d utilità
014200041110     c                   Call(e)   'TRUL82R'
014300041110     c                   Parm                    Trul82ds
014400041110
014500041110     c     cponrr        Chain(e)  Tncpo00f
014600041110      * se è ancora allocato vado a fine lavoro e risottometto l'azione
014700041110    2c                   If        %Error
014800041110     c                   Eval      wbatch = '1'
014900041110     c                   Goto      Fine
015000041110     c                   endif
015100041110     c                   endif
015200160804      /free
015300160804       //?Salvo l'immagine precedente del potenziale
015400160804         exsr ImmaginePrima;
015500160804      /end-free
015600041110
015700950828     C                   Z-ADD     CCMN          CPOCMM
015800040806     c                   z-add     dateu         CPODTR
015900950828     C                   EXCEPT    AGGCPO
016000160804      /free
016100160804       //?Scrivo la variazione fatta sul potenziale
016200160804         exsr ScriviVariazione;
016300160804      /end-free
016400950828     C                   END
016500981201     C     CCMV          READE     TNCPO08L                               30
016600950828     C                   ENDDO
016700950829     C******
016800950828     C* CAMBIO CODICE COMMERCIALE SU CNCLP
016900950829     C******
017000020902     C                   Z-ADD     DutKci        KKCC
017100950828     C     KCLP          SETLL     CNCLP01L
017200950828     C     KCLP          READE     CNCLP01L                               30
017300950828     C     *IN30         DOWEQ     '0'
017400950828     C     CLPATB        IFNE      'A'
017500960906     C* Escludo tariffe di cartello
017600960906     C     STRKSC        ANDNE     88888
017700050201
017800050201      * Gestisco allocazione del record con invio del msg all'utente che blocca il rcd
017900050201     c     clpnrr        Chain(e)  Cnclp00f
018000050201    2c                   If        %Error
018100050201     c                   Clear                   Trul82ds
018200050201     c                   Eval      ul82§rrn = clpnrr
018300050201     c                   Eval      UL82§FIL = 'CNCLP00F'
018400050201     c                   Eval      UL82§WIN = 'N'
018500050201     c                   Eval      UL82§F7  = 'S'
018600050201     c                   Eval      UL82§num = 5
018700050201     c                   Eval      UL82§att = 60
018800050201     c                   Eval      UL82§mss = msgjob
018900050201      * Effettuo la chiamata al *PGM d utilità
019000050201     c                   Call(e)   'TRUL82R'
019100050201     c                   Parm                    Trul82ds
019200050201
019300050201     c     clpnrr        Chain(e)  Cnclp00f
019400050201      * se è ancora allocato vado a fine lavoro e risottometto l'azione
019500050201    2c                   If        %Error
019600050201     c                   Eval      wbatch = '1'
019700050201     c                   Goto      Fine
019800050201     c                   endif
019900050201     c                   endif
020000050201
020100950828     C                   Z-ADD     CCMN          CLPAGE
020200950828     C                   EXCEPT    AGGCLP
020300950828     C     KACO          CHAIN     CNACO00F                           30
020400950828     C  N30              MOVE      *BLANKS       ACOFTR
020500950830     C  N30              MOVE      *ZEROS        ACODTR
020600950828     C  N30              EXCEPT    AGGACO
020700950828     C                   END
020800950828     C     KCLP          READE     CNCLP01L                               30
020900950828     C                   ENDDO
021000950829     C******
021100950829     C* CAMBIO CODICE COMMERCIALE SU TFCLP
021200950829     C******
021300020902     C                   Z-ADD     DutKci        KKCC
021400050927     C     KFCLP         SETLL     TFCLP000
021500050927     C     KFCLP         READE     TFCLP000                               30
021600950829     C     *IN30         DOWEQ     '0'
021700950829     C     CLPATB        IFNE      'A'
021800950829     C                   Z-ADD     CCMN          CLPAGE
021900950829     C                   EXCEPT    AGFCLP
022000950829     C                   END
022100050927     C     KFCLP         READE     TFCLP000                               30
022200950829     C                   ENDDO
022300950829     C******
022400950828     C* CAMBIO CODICE COMMERCIALE SU TFSAC
022500950829     C******
022600950830     C                   MOVE      *BLANKS       COMSAC            1
022700950830     C*
022800950830     C     CCMV          SETLL     TFSAC01L
022900950830     C     CCMV          READE     TFSAC01L                               30
023000950904     C     *IN30         DOWEQ     *OFF
023100950830     C* Verifico se esiste già un record con stessa chiave attribuito
023200950830     C* al codice commerciale nuovo
023300950830     C     KSAC          SETLL     TFSAC01L                               30
023400950829     C     *IN30         IFEQ      *ON
023500950830     C* Record trovato: lo aggiorno sommando i dati del vecchio codice
023600950830     C* commerciale e deleto il record col vecchio ccm
023700950830     C                   MOVEL     SACDS         SACMEM
023800950831     C     KSAC1         DELETE    TFSAC000                           30
023900950830     C     KSAC          CHAIN     TFSAC01L                           30
024000950830     C                   ADD       MCTO1         SACTO1
024100950830     C                   ADD       MCTO2         SACTO2
024200950830     C                   ADD       MCTO3         SACTO3
024300950830     C                   ADD       MCTO4         SACTO4
024400041011     c                   z-add     dateu         SACDTR
024500950830     C                   UPDATE    TFSAC000
024600950830     C     KSAC1         SETLL     TFSAC01L                                     Riposizion.
024700950830     C                   ELSE
024800950830     C* Se non esiste aggiorno il codice commerciale sul record vecchio
024900950830     C     KSAC1         CHAIN     TFSAC01L                           30
025000950830     C                   Z-ADD     CCMN          SACCMM
025100041011     c                   z-add     dateu         SACDTR
025200950830     C                   EXCEPT    AGGSAC
025300950829     C                   END
025400950830     C*
025500950830     C     CCMV          READE     TFSAC01L                               30
025600950828     C                   ENDDO
025700101115
025800101115      /free
025900101115       //?Modifica commerciali su Attività e su Trattative
026000101115
026100101115       //?ATTIVITA'
026200101115
026300101115       //?Come prima cosa elimino tutte le attività Ferie e Ufficio
026400101115         exec sql
026500101115           DELETE  from  TIATC00F
026600101115           where  ATCTAT in('F', 'U') and  ATCCMM = :ccmv;
026700101115
026800101115       //?Aggiorno TUTTE le attività NON LEGATE a trattativa
026900101115         exec sql
027000101115           UPDATE  TIATC00F  set ATCCMM = :ccmn
027100101115           where  ATCCMM = :ccmv  and  ATCnrv = 0;
027200101115
027300101115         exec sql
027400101115           UPDATE  TIATC00F  set ATCCCO = :ccmn
027500101115           where  ATCCCO = :ccmv  and  ATCnrv = 0;
027600101115
027700101115         exec sql
027800101115           UPDATE  TIATC00F  set ATCCO3 = :ccmn
027900101115           where  ATCCO3 = :ccmv  and  ATCnrv = 0;
028000101115
028100101115       //?Aggiorno le attività LEGATE a trattativa
028200101115       //?Aggiorno TUTTE le attività CHIUSE
028300101115         exec sql
028400101115           UPDATE  TIATC00F  set ATCCMM = :ccmn
028500101115           where  ATCCMM = :ccmv  and  ATCnrv > 0 and
028600101115                  ATCdco > 0;
028700101115
028800101115         exec sql
028900101115           UPDATE  TIATC00F  set ATCCCO = :ccmn
029000101115           where  ATCCCO = :ccmv  and  ATCnrv > 0 and
029100101115                  ATCdco > 0;
029200101115
029300101115         exec sql
029400101115           UPDATE  TIATC00F  set ATCCO3 = :ccmn
029500101115           where  ATCCO3 = :ccmv  and  ATCnrv > 0 and
029600101115                  ATCdco > 0;
029700101115
029800101115       //?Aggiorno le attività APERTE SOLO se comm. trattativa <> CCMV
029900101115       //?commerciale attività
030000101115         $EndF1 = *off;
030100101115         exec sql
030200101115           DECLARE  F1 cursor for
030300101115           select rrn(TIATC00F) as w_ATCrcd, TIATC00F.* from TIATC00F
030400101115           where  ATCCMM = :ccmv  and  ATCnrv > 0  and
030500101115                  ATCdco = 0
030600101115           order by  ATCnrv;
030700101115
030800101115         exec sql
030900101115           open F1;
031000101115           IF sqlcode < 0;
031100101115             $EndF1 = *on;
031200101115           ENDIF;
031300101115
031400101115         DOW not $EndF1;
031500101115           exec sql
031600101115             fetch next from F1 into :w_ATCrcd, :TIATCds;
031700101115           IF sqlcod = 100 or sqlcod < 0;
031800101115             $EndF1 = *on;
031900101115             leave;
032000101115           ENDIF;
032100101115         //?Aggancio la trattativa, il commerciale gestione trattativa
032200101115         //?NON deve essere il commerciale OLD
032300101115           chain  w_ATCnrv  TIVIS05L;
032400101115           IF  %found(TIVIS05L) and  VIScmm = ccmv;
032500101115             iter;
032600101115           ENDIF;
032700101115           IF  not %found(TIVIS05L);
032800101115             iter;
032900101115           ENDIF;
033000101115         //?Aggiorno il commerciale sull'attività
033100101115         //?gestisco allocazione rcd
033200101115           chain(e)  w_ATCrcd  TIATC00F;
033300101115           IF  %error;
033400101115             clear trul82ds;
033500101115             UL82§rrn = w_ATCrcd;
033600101115             UL82§fil = 'TIATC00F';
033700101115             UL82§win = 'N';
033800101115             UL82§f7  = 'S';
033900101115             UL82§num = 5;
034000101115             UL82§att = 60;
034100101115             UL82§mss = msgjob;
034200101115         //?richiamo il pgm
034300101115             callp(e) trul82r (trul82ds);
034400101115         //?se il rcd è ancora allocato vado a fine lavoro e risottometto l'azione
034500101115             chain(e) w_ATCrcd  TIATC00F;
034600101115             IF  %error;
034700101115               exsr Riprova;
034800101115             ENDIF;
034900101115           ENDIF;
035000101115         //?se il rcd non è più allocato aggiorno il commerciale
035100101115           ATCcmm = ccmn;
035200101115           update TIATC000;
035300101115         ENDDO;
035400101115         exec sql close F1;
035500101115
035600101115       //?3° commerciale
035700101115         $EndF2 = *off;
035800101115         exec sql
035900101115           DECLARE  F2 cursor for
036000101115           select rrn(TIATC00F) as w_ATCrcd, TIATC00F.* from TIATC00F
036100101115           where  ATCCO3 = :ccmv  and  ATCnrv > 0  and
036200101115                  ATCdco = 0
036300101115           order by  ATCnrv;
036400101115
036500101115         exec sql
036600101115           open F2;
036700101115           IF sqlcode < 0;
036800101115             $EndF2 = *on;
036900101115           ENDIF;
037000101115
037100101115         DOW not $EndF2;
037200101115           exec sql
037300101115             fetch next from F2 into :w_ATCrcd, :TIATCds;
037400101115           IF sqlcod = 100 or sqlcod < 0;
037500101115             $EndF2 = *on;
037600101115             leave;
037700101115           ENDIF;
037800101115         //?Aggancio la trattativa, il commerciale gestione trattativa
037900101115         //?NON deve essere il commerciale OLD
038000101115           chain  w_ATCnrv  TIVIS05L;
038100101115           IF  %found(TIVIS05L) and  VIScmm = ccmv;
038200101115             iter;
038300101115           ENDIF;
038400101115           IF  not %found(TIVIS05L);
038500101115             iter;
038600101115           ENDIF;
038700101115         //?Aggiorno il commerciale sull'attività
038800101115         //?gestisco allocazione rcd
038900101115           chain(e)  w_ATCrcd  TIATC00F;
039000101115           IF  %error;
039100101115             clear trul82ds;
039200101115             UL82§rrn = w_ATCrcd;
039300101115             UL82§fil = 'TIATC00F';
039400101115             UL82§win = 'N';
039500101115             UL82§f7  = 'S';
039600101115             UL82§num = 5;
039700101115             UL82§att = 60;
039800101115             UL82§mss = msgjob;
039900101115         //?richiamo il pgm
040000101115             callp(e) trul82r (trul82ds);
040100101115         //?se il rcd è ancora allocato vado a fine lavoro e risottometto l'azione
040200101115             chain(e)  w_ATCrcd  TIATC00F;
040300101115             IF  %error;
040400101115               exsr Riprova;
040500101115             ENDIF;
040600101115           ENDIF;
040700101115         //?se il rcd non è più allocato aggiorno il commerciale
040800101115           ATCco3 = ccmn;
040900101115           update TIATC000;
041000101115         ENDDO;
041100101115         exec sql close F2;
041200101115
041300101115       //?TRATTATIVE fittizie e non
041400101115
041500101115       //?Aggiorno il commerciale apertura trattativa su TUTTE
041600101115         exec sql
041700101115           UPDATE  TIVIS00F  set VISCMMI = :ccmn
041800101115           where  VISCMMI = :ccmv;
041900101115
042000101115       //?Aggiorno il commerciale gestione trattativa su TUTTE le CHIUSE
042100101115         exec sql
042200101115           UPDATE  TIVIS00F  set VISCMM = :ccmn
042300101115           where  VISCMM = :ccmv and VISDCH > 0;
042400101115
042500101115       //?Aggiorno il commerciale gestione trattativa sulle APERTE
042600101115       //?solo se NON eiste già un'altra trattativa aperta su stesso cpo/ksc/cmm
042700101115       //?se la trattativa NON è fittizia
042800101115       //?per trattativa fittizia non controllo
042900101115         $EndF3 = *off;
043000101115         exec sql
043100101115           DECLARE  F3 cursor for
043200101115           select rrn(TIVIS00F) as w_VISrcd, TIVIS00F.* from TIVIS00F
043300101115           where  VISCMM = :ccmv  and  VISdch = 0
043400101115           order by  VISnrv;
043500101115
043600101115         exec sql
043700101115           open F3;
043800101115           IF sqlcode < 0;
043900101115             $EndF3 = *on;
044000101115           ENDIF;
044100101115
044200101115         DOW not $EndF3;
044300101115           exec sql
044400101115             fetch next from F3 into :w_VISrcd, :TIVISds;
044500101115           IF sqlcod = 100 or sqlcod < 0;
044600101115             $EndF3 = *on;
044700101115             leave;
044800101115           ENDIF;
044900101115
045000101115         //?Controllo che con il nuovo commerciale non esista già un'altra trattative
045100101115         //?CPO/KSC/CMM, se trattativa NON fittizia
045200101115           IF  w_VISffz = *blanks;
045300101115             clear tnta43ds;
045400101115             ITA43cpo = w_VIScpo;
045500101115             ITA43nrv = w_VISnrv;
045600101115             ITA43ksc = w_VISksc;
045700101115             ITA43cmm = ccmn;
045800101115             tnta43r (tnta43ds);
045900101115           //?trovo altra trattativa stampo segnalazione
046000101115             IF  OTA43nrv > 0;
046100101115               exsr stampa;
046200101115               iter;
046300101115             ENDIF;
046400101115           ENDIF;
046500101115
046600101115         //?Aggiorno il commerciale gestione trattativa
046700101115         //?gestisco allocazione rcd
046800101115           chain(e)  w_VISrcd  TIVIS00F;
046900101115           IF  %error;
047000101115             clear trul82ds;
047100101115             UL82§rrn = w_VISrcd;
047200101115             UL82§fil = 'TIVIS00F';
047300101115             UL82§win = 'N';
047400101115             UL82§f7  = 'S';
047500101115             UL82§num = 5;
047600101115             UL82§att = 60;
047700101115             UL82§mss = msgjob;
047800101115         //?richiamo il pgm
047900101115             callp(e) trul82r (trul82ds);
048000101115         //?se il rcd è ancora allocato vado a fine lavoro e risottometto l'azione
048100101115             chain(e)  w_VISrcd  TIVIS00F;
048200101115             IF  %error;
048300101115               exsr Riprova;
048400101115             ENDIF;
048500101115           ENDIF;
048600101115         //?se il rcd non è più allocato aggiorno il commerciale
048700101115           VIScmm = ccmn;
048800101115           update TIVIS000;
048900101115         //?Aggiorno anche le relative attività legate
049000101115           exsr Attivita;
049100101115         ENDDO;
049200101115         exec sql close F3;
049300101115
049400101115      /end-free
049500101005
049600100519     c
049700100519     c
049800041110     c     Fine          Tag
049900041110     c                   If        wbatch = *On
050000041110     c                   Eval      Kpjbu = Param
050100041110     c                   Eval      Kcoaz = 'MK45'
050200041110     c                   Call      'BCH10'
050300041110     c                   Parm                    KPJBA
050400041110     c                   EndIf
050500160804
050600160804      /free
050700160804       //?Chiudo file storicizzazione variazioni potenziale
050800160804         clear TRMK30DS;
050900160804         clear tncpo_30;
051000160804         clear tncpo1_30;
051100160804         clear ticpi_30;
051200160804         IMK30tla = 'C';
051300160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
051400160804      /end-free
051500971125     C*
051600950828     C                   SETON                                        LR
051700101115       //--------------------------------------------------------------
051800101115       //?Risottometto l'azione ed esco dal pgm.
051900101115       //--------------------------------------------------------------
052000101115     c     Riprova       begsr
052100101115
052200101115     c                   Eval      Kpjbu = Param
052300101115     c                   Eval      Kcoaz = 'MK45'
052400101115     c                   Call      'BCH10'
052500101115     c                   Parm                    KPJBA
052600101115
052700101115     c                   eval      *inlr = *on
052800101115     c                   return
052900101115
053000101115     c                   endsr
053100101115
053200101115       //--------------------------------------------------------------
053300101115       //?Stampo la segnalazione di errore.
053400101115       //--------------------------------------------------------------
053500101115     c     Stampa        begsr
053600101115
053700101115     c                   if        testata = *blanks
053800101115     c                   except    TESTA
053900101115     c                   eval      testata = 'S'
054000101115     c                   endif
054100101115
054200101115     c                   except    STAERR
054300101115
054400101115     c                   endsr
054500101115
054600101115      /free
054700160804       //--------------------------------------------------------------
054800160804       //?Scrivo l'immagine Precedente anagrafica Potenziale
054900160804       //--------------------------------------------------------------
055000160804       BEGSR ImmaginePrima;
055100160804
055200160804         clear TRMK30DS;
055300160804         IMK30immag = 'P';
055400160804         IMK30tvp = 'E';
055500160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
055600160804
055700160804       ENDSR;
055800160804
055900160804       //--------------------------------------------------------------
056000160804       //?Scrivo Storico Variazioni.
056100160804       //--------------------------------------------------------------
056200160804       BEGSR ScriviVariazione;
056300160804
056400160804         clear TRMK30DS;
056500160804         IMK30pru = knmus;
056600160804         IMK30noj = knmeb;
056700160804         IMK30pgm = 'TRMK45R';
056800160804         IMK30immag = 'D';
056900160804         IMK30cta = 'M';
057000160804         IMK30tvp = 'E';
057100160804
057200160804         trmk30r (trmk30ds:tncpo_30:tncpo1_30:ticpi_30);
057300160804
057400160804       ENDSR;
057500160804
057600101115       //--------------------------------------------------------------
057700101115       //?Aggiorno le attività legate alla trattativa aperta
057800101115       //--------------------------------------------------------------
057900101115       BEGSR Attivita;
058000101115
058100101115         exec sql
058200101115           UPDATE  TIATC00F  set ATCCMM = :ccmn
058300101115           where  ATCCMM = :ccmv  and  ATCnrv = :VISnrv;
058400101115
058500101115         exec sql
058600101115           UPDATE  TIATC00F  set ATCCCO = :ccmn
058700101115           where  ATCCCO = :ccmv  and  ATCnrv = :VISnrv;
058800101115
058900101115         exec sql
059000101115           UPDATE  TIATC00F  set ATCCO3 = :ccmn
059100101115           where  ATCCO3 = :ccmv  and  ATCnrv = :VISnrv;
059200101115
059300101115       ENDSR;
059400101115      /end-free
059500101115
059600950828     C*****************************************************************
059700950828     C     *INZSR        BEGSR
059800950828     C*
059900950825     C     *ENTRY        PLIST
060000950825     C                   PARM                    KPJBA
060100950825     C                   MOVEL     KPJBU         PARAM
060200040806
060300040806      * data del giorno
060400040806     c                   time                    w0140            14 0
060500040806      * UDATE IN GGMMAAAA
060600040806     c                   MOVE      W0140         WDTGIO            8 0
060700040806
060800040806      * UDATE IN AAAAMMGG
060900040806     c                   Z-ADD     WDTGIO        G08DAT
061000040806     c                   MOVEL     *BLANK        G08ERR
061100040806     c                   CALL      'XSRDA8'
061200040806     c                   PARM                    WLBDA8
061300040806     c                   MOVEL     G08INV        DATEU             8 0
061400020902      *
061500020902      * Reperisco dati job
061600020902     C                   EXSR      DatiJob
061700950829     C*
061800020902     C                   Z-ADD     1             CODUT             1 0
061900981204     C**
062000981204     C     SIMFEL        CHAIN     AZORG01L                           31
062100981204     C  N31              MOVEL     ORGFEL        WSFEL
062200981204     C   31              MOVEL     SIMFEL        WSFEL
062300981204     C     *LIKE         DEFINE    SIMFEL        WSFEL
062400950828     C*
062500950828     C                   MOVE      PACCMV        CCMV
062600950828     C                   MOVE      PACCMN        CCMN
062700950828     C*
062800950828     C     KCLP          KLIST
062900950829     C                   KFLD                    CODUT
063000950828     C                   KFLD                    KKCC
063100950828     C                   KFLD                    CCMV
063200950828     C*
063300950828     C     KACO          KLIST
063400950828     C                   KFLD                    CLPKUT
063500950828     C                   KFLD                    CLPKCC
063600950828     C                   KFLD                    CLPKSC
063700950828     C*
063800950829     C     KFCLP         KLIST
063900950830     C                   KFLD                    CODUT
064000950829     C                   KFLD                    KKCC
064100050927     C                   KFLD                    ccmv
064200950830     C*
064300950830     C     KSAC          KLIST
064400950830     C                   KFLD                    CCMN
064500950830     C                   KFLD                    SACAAA
064600950830     C                   KFLD                    SACMMM
064700950830     C                   KFLD                    SACAPL
064800950830     C                   KFLD                    SACTPV
064900950830     C                   KFLD                    SACCLV
065000981029     C                   KFLD                    SACFIE
065100981029     C                   KFLD                    SACFFZ
065200950830     C*
065300950830     C     KSAC1         KLIST
065400950830     C                   KFLD                    CCMV
065500950830     C                   KFLD                    SACAAA
065600950830     C                   KFLD                    SACMMM
065700950830     C                   KFLD                    SACAPL
065800950830     C                   KFLD                    SACTPV
065900950830     C                   KFLD                    SACCLV
066000981029     C                   KFLD                    SACFIE
066100981029     C                   KFLD                    SACFFZ
066200950828     C*
066300950828     C     *LIKE         DEFINE    CLPKCC        KKCC
066400950828     C     *LIKE         DEFINE    CPOCMM        CCMV
066500950828     C     *LIKE         DEFINE    CPOCMM        CCMN
066600950828     C*
066700950828     C                   ENDSR
066800020902      *----------------------------------------------------------------
066900020902      *  Reperisco Dati del job (Utente/Operativi)
067000020902      *----------------------------------------------------------------
067100020902     C     DatiJob       BegSr
067200020902      *
067300020902     C     *dtaara       define    §azute        azuteds
067400020902     C     *dtaara       define    §datiute      ddatiute
067500020902      *
067600020902     C                   in(E)     *dtaara
067700020902     C                   IF        %Error or RSUT = *blanks
067800020902     C                   call      'TIBS34R'
067900020902     C                   parm                    Tibs34Ds
068000020902     C                   in        *dtaara
068100020902     C                   ENDIF
068200020902     C                   endsr
068300020902     C*
068400020902     C*---------------------------------------------------------------*
068500041110     OTNCPO00   E            AGGCPO
068600950828     O                       CPOCMM
068700950830     O                       CPODTR
068800050201     OCNCLP00   E            AGGCLP
068900950828     O                       CLPAGE
069000950828     OCNACO000  E            AGGACO
069100950828     O                       ACOFTR
069200950830     O                       ACODTR
069300950828     OTFSAC000  E            AGGSAC
069400950828     O                       SACCMM
069500041011     O                       SACDTR
069600050927     OTFCLP000  E            AGFCLP
069700950828     O                       CLPAGE
069800100519     O
069900101115
070000100520     OQSYSPRT   E            TESTA            02 04
070100100520     O                                        +  20 '*** CAMBIO CODICE '
070200101115     O                                        +   1 'COMMERCIALE da '
070300100519     O                       CCMV             +   1
070400100520     O                                        +   1 'a '
070500100519     O                       CCMN             +   1
070600100520     O                                        +   1 '***'
070700100519     O                       *date              118 '  /  /    '
070800101115     O          E            staERR      2
070900100519     O                                              'TRATTATIVA '
071000101115     O                       w_VISnrv      Z   +  1
071100101116     O                                         +  1 'non aggiornata: già '
071200101116     O                                         +  1 'presente la trattativa '
071300101115     O                       OTA43nrv      Z   +  1
071400101116     O                                         +  1 'su stesso potenziale -'
071500100519     O                                         +  1 'cliente'
