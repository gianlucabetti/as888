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
007600041110
007700041110     d cpoinf          ds
007800041110     d  cponrr               397    400i 0
007900050201
008000050201     d clpinf          ds
008100050201     d  clpnrr               397    400i 0
008200040806
008300040806     D WLBDA8          DS
008400040806     D  G08DAT                 1      8  0
008500040806     D  G08INV                 9     16  0
008600040806     D  G08ERR                17     17
008700040806     D  G08TGI                18     22  0
008800041110
008900041110     d msgjob          C                   CONST('Si sta bloccando l''aggior.de-
009000041110     d                                     i codici commerciali:USCIRE SUBITO d-
009100041110     d                                     a questo lavoro')
009200101115
009300101115      //---------------------------------------------------------------
009400101115      //?Definizione procedure esterne.
009500101115      //---------------------------------------------------------------
009600101115     d trul82r         pr                  extpgm('TRUL82R')
009700101115     d  trul82ds                           likeds(TRUL82DS)
009800101115
009900101115     D Tnta43R         pr                  extpgm('TNTA43R')
010000101115     D  tnta43ds                           likeds(tnta43ds)
010100101115
010200950831     C****************************************************************
010300950831     C*  RIEPILOGO INDICATORI
010400950831     C***************************************************************
010500950831     C* 20    - PER QCMDEXC
010600950831     C* 30    - COMODO
010700950829     C******
010800981201     C* CAMBIO CODICE COMMERCIALE SU TNCPO
010900981204     C*  NON HO BISOGNO DI TESTARE SE E' DEL MIO SISTEMA
011000981204     C*  PERCHE' IL COD COMMERCIALE DEVE ESSERE DELL'AREA PER GESTIRLO
011100950829     C******
011200981201     C     CCMV          SETLL     TNCPO08L
011300981201     C     CCMV          READE     TNCPO08L                               30
011400950828     C     *IN30         DOWEQ     '0'
011500950828     C     CPOATB        IFNE      'A'
011600041110
011700041110      * Gestisco allocazione del record con invio del msg all'utente che blocca il rcd
011800041110     c     cponrr        Chain(e)  Tncpo00f
011900041110    2c                   If        %Error
012000041110     c                   Clear                   Trul82ds
012100041110     c                   Eval      ul82§rrn = cponrr
012200041110     c                   Eval      UL82§FIL = 'TNCPO00F'
012300041110     c                   Eval      UL82§WIN = 'N'
012400041110     c                   Eval      UL82§F7  = 'S'
012500041110     c                   Eval      UL82§num = 5
012600041110     c                   Eval      UL82§att = 60
012700041110     c                   Eval      UL82§mss = msgjob
012800041110      * Effettuo la chiamata al *PGM d utilità
012900041110     c                   Call(e)   'TRUL82R'
013000041110     c                   Parm                    Trul82ds
013100041110
013200041110     c     cponrr        Chain(e)  Tncpo00f
013300041110      * se è ancora allocato vado a fine lavoro e risottometto l'azione
013400041110    2c                   If        %Error
013500041110     c                   Eval      wbatch = '1'
013600041110     c                   Goto      Fine
013700041110     c                   endif
013800041110     c                   endif
013900041110
014000950828     C                   Z-ADD     CCMN          CPOCMM
014100040806     c                   z-add     dateu         CPODTR
014200950828     C                   EXCEPT    AGGCPO
014300950828     C                   END
014400981201     C     CCMV          READE     TNCPO08L                               30
014500950828     C                   ENDDO
014600950829     C******
014700950828     C* CAMBIO CODICE COMMERCIALE SU CNCLP
014800950829     C******
014900020902     C                   Z-ADD     DutKci        KKCC
015000950828     C     KCLP          SETLL     CNCLP01L
015100950828     C     KCLP          READE     CNCLP01L                               30
015200950828     C     *IN30         DOWEQ     '0'
015300950828     C     CLPATB        IFNE      'A'
015400960906     C* Escludo tariffe di cartello
015500960906     C     STRKSC        ANDNE     88888
015600050201
015700050201      * Gestisco allocazione del record con invio del msg all'utente che blocca il rcd
015800050201     c     clpnrr        Chain(e)  Cnclp00f
015900050201    2c                   If        %Error
016000050201     c                   Clear                   Trul82ds
016100050201     c                   Eval      ul82§rrn = clpnrr
016200050201     c                   Eval      UL82§FIL = 'CNCLP00F'
016300050201     c                   Eval      UL82§WIN = 'N'
016400050201     c                   Eval      UL82§F7  = 'S'
016500050201     c                   Eval      UL82§num = 5
016600050201     c                   Eval      UL82§att = 60
016700050201     c                   Eval      UL82§mss = msgjob
016800050201      * Effettuo la chiamata al *PGM d utilità
016900050201     c                   Call(e)   'TRUL82R'
017000050201     c                   Parm                    Trul82ds
017100050201
017200050201     c     clpnrr        Chain(e)  Cnclp00f
017300050201      * se è ancora allocato vado a fine lavoro e risottometto l'azione
017400050201    2c                   If        %Error
017500050201     c                   Eval      wbatch = '1'
017600050201     c                   Goto      Fine
017700050201     c                   endif
017800050201     c                   endif
017900050201
018000950828     C                   Z-ADD     CCMN          CLPAGE
018100950828     C                   EXCEPT    AGGCLP
018200950828     C     KACO          CHAIN     CNACO00F                           30
018300950828     C  N30              MOVE      *BLANKS       ACOFTR
018400950830     C  N30              MOVE      *ZEROS        ACODTR
018500950828     C  N30              EXCEPT    AGGACO
018600950828     C                   END
018700950828     C     KCLP          READE     CNCLP01L                               30
018800950828     C                   ENDDO
018900950829     C******
019000950829     C* CAMBIO CODICE COMMERCIALE SU TFCLP
019100950829     C******
019200020902     C                   Z-ADD     DutKci        KKCC
019300050927     C     KFCLP         SETLL     TFCLP000
019400050927     C     KFCLP         READE     TFCLP000                               30
019500950829     C     *IN30         DOWEQ     '0'
019600950829     C     CLPATB        IFNE      'A'
019700950829     C                   Z-ADD     CCMN          CLPAGE
019800950829     C                   EXCEPT    AGFCLP
019900950829     C                   END
020000050927     C     KFCLP         READE     TFCLP000                               30
020100950829     C                   ENDDO
020200950829     C******
020300950828     C* CAMBIO CODICE COMMERCIALE SU TFSAC
020400950829     C******
020500950830     C                   MOVE      *BLANKS       COMSAC            1
020600950830     C*
020700950830     C     CCMV          SETLL     TFSAC01L
020800950830     C     CCMV          READE     TFSAC01L                               30
020900950904     C     *IN30         DOWEQ     *OFF
021000950830     C* Verifico se esiste già un record con stessa chiave attribuito
021100950830     C* al codice commerciale nuovo
021200950830     C     KSAC          SETLL     TFSAC01L                               30
021300950829     C     *IN30         IFEQ      *ON
021400950830     C* Record trovato: lo aggiorno sommando i dati del vecchio codice
021500950830     C* commerciale e deleto il record col vecchio ccm
021600950830     C                   MOVEL     SACDS         SACMEM
021700950831     C     KSAC1         DELETE    TFSAC000                           30
021800950830     C     KSAC          CHAIN     TFSAC01L                           30
021900950830     C                   ADD       MCTO1         SACTO1
022000950830     C                   ADD       MCTO2         SACTO2
022100950830     C                   ADD       MCTO3         SACTO3
022200950830     C                   ADD       MCTO4         SACTO4
022300041011     c                   z-add     dateu         SACDTR
022400950830     C                   UPDATE    TFSAC000
022500950830     C     KSAC1         SETLL     TFSAC01L                                     Riposizion.
022600950830     C                   ELSE
022700950830     C* Se non esiste aggiorno il codice commerciale sul record vecchio
022800950830     C     KSAC1         CHAIN     TFSAC01L                           30
022900950830     C                   Z-ADD     CCMN          SACCMM
023000041011     c                   z-add     dateu         SACDTR
023100950830     C                   EXCEPT    AGGSAC
023200950829     C                   END
023300950830     C*
023400950830     C     CCMV          READE     TFSAC01L                               30
023500950828     C                   ENDDO
023600101115
023700101115      /free
023800101115       //?Modifica commerciali su Attività e su Trattative
023900101115
024000101115       //?ATTIVITA'
024100101115
024200101115       //?Come prima cosa elimino tutte le attività Ferie e Ufficio
024300101115         exec sql
024400101115           DELETE  from  TIATC00F
024500101115           where  ATCTAT in('F', 'U') and  ATCCMM = :ccmv;
024600101115
024700101115       //?Aggiorno TUTTE le attività NON LEGATE a trattativa
024800101115         exec sql
024900101115           UPDATE  TIATC00F  set ATCCMM = :ccmn
025000101115           where  ATCCMM = :ccmv  and  ATCnrv = 0;
025100101115
025200101115         exec sql
025300101115           UPDATE  TIATC00F  set ATCCCO = :ccmn
025400101115           where  ATCCCO = :ccmv  and  ATCnrv = 0;
025500101115
025600101115         exec sql
025700101115           UPDATE  TIATC00F  set ATCCO3 = :ccmn
025800101115           where  ATCCO3 = :ccmv  and  ATCnrv = 0;
025900101115
026000101115       //?Aggiorno le attività LEGATE a trattativa
026100101115       //?Aggiorno TUTTE le attività CHIUSE
026200101115         exec sql
026300101115           UPDATE  TIATC00F  set ATCCMM = :ccmn
026400101115           where  ATCCMM = :ccmv  and  ATCnrv > 0 and
026500101115                  ATCdco > 0;
026600101115
026700101115         exec sql
026800101115           UPDATE  TIATC00F  set ATCCCO = :ccmn
026900101115           where  ATCCCO = :ccmv  and  ATCnrv > 0 and
027000101115                  ATCdco > 0;
027100101115
027200101115         exec sql
027300101115           UPDATE  TIATC00F  set ATCCO3 = :ccmn
027400101115           where  ATCCO3 = :ccmv  and  ATCnrv > 0 and
027500101115                  ATCdco > 0;
027600101115
027700101115       //?Aggiorno le attività APERTE SOLO se comm. trattativa <> CCMV
027800101115       //?commerciale attività
027900101115         $EndF1 = *off;
028000101115         exec sql
028100101115           DECLARE  F1 cursor for
028200101115           select rrn(TIATC00F) as w_ATCrcd, TIATC00F.* from TIATC00F
028300101115           where  ATCCMM = :ccmv  and  ATCnrv > 0  and
028400101115                  ATCdco = 0
028500101115           order by  ATCnrv;
028600101115
028700101115         exec sql
028800101115           open F1;
028900101115           IF sqlcode < 0;
029000101115             $EndF1 = *on;
029100101115           ENDIF;
029200101115
029300101115         DOW not $EndF1;
029400101115           exec sql
029500101115             fetch next from F1 into :w_ATCrcd, :TIATCds;
029600101115           IF sqlcod = 100 or sqlcod < 0;
029700101115             $EndF1 = *on;
029800101115             leave;
029900101115           ENDIF;
030000101115         //?Aggancio la trattativa, il commerciale gestione trattativa
030100101115         //?NON deve essere il commerciale OLD
030200101115           chain  w_ATCnrv  TIVIS05L;
030300101115           IF  %found(TIVIS05L) and  VIScmm = ccmv;
030400101115             iter;
030500101115           ENDIF;
030600101115           IF  not %found(TIVIS05L);
030700101115             iter;
030800101115           ENDIF;
030900101115         //?Aggiorno il commerciale sull'attività
031000101115         //?gestisco allocazione rcd
031100101115           chain(e)  w_ATCrcd  TIATC00F;
031200101115           IF  %error;
031300101115             clear trul82ds;
031400101115             UL82§rrn = w_ATCrcd;
031500101115             UL82§fil = 'TIATC00F';
031600101115             UL82§win = 'N';
031700101115             UL82§f7  = 'S';
031800101115             UL82§num = 5;
031900101115             UL82§att = 60;
032000101115             UL82§mss = msgjob;
032100101115         //?richiamo il pgm
032200101115             callp(e) trul82r (trul82ds);
032300101115         //?se il rcd è ancora allocato vado a fine lavoro e risottometto l'azione
032400101115             chain(e) w_ATCrcd  TIATC00F;
032500101115             IF  %error;
032600101115               exsr Riprova;
032700101115             ENDIF;
032800101115           ENDIF;
032900101115         //?se il rcd non è più allocato aggiorno il commerciale
033000101115           ATCcmm = ccmn;
033100101115           update TIATC000;
033200101115         ENDDO;
033300101115         exec sql close F1;
033400101115
033500101115       //?3° commerciale
033600101115         $EndF2 = *off;
033700101115         exec sql
033800101115           DECLARE  F2 cursor for
033900101115           select rrn(TIATC00F) as w_ATCrcd, TIATC00F.* from TIATC00F
034000101115           where  ATCCO3 = :ccmv  and  ATCnrv > 0  and
034100101115                  ATCdco = 0
034200101115           order by  ATCnrv;
034300101115
034400101115         exec sql
034500101115           open F2;
034600101115           IF sqlcode < 0;
034700101115             $EndF2 = *on;
034800101115           ENDIF;
034900101115
035000101115         DOW not $EndF2;
035100101115           exec sql
035200101115             fetch next from F2 into :w_ATCrcd, :TIATCds;
035300101115           IF sqlcod = 100 or sqlcod < 0;
035400101115             $EndF2 = *on;
035500101115             leave;
035600101115           ENDIF;
035700101115         //?Aggancio la trattativa, il commerciale gestione trattativa
035800101115         //?NON deve essere il commerciale OLD
035900101115           chain  w_ATCnrv  TIVIS05L;
036000101115           IF  %found(TIVIS05L) and  VIScmm = ccmv;
036100101115             iter;
036200101115           ENDIF;
036300101115           IF  not %found(TIVIS05L);
036400101115             iter;
036500101115           ENDIF;
036600101115         //?Aggiorno il commerciale sull'attività
036700101115         //?gestisco allocazione rcd
036800101115           chain(e)  w_ATCrcd  TIATC00F;
036900101115           IF  %error;
037000101115             clear trul82ds;
037100101115             UL82§rrn = w_ATCrcd;
037200101115             UL82§fil = 'TIATC00F';
037300101115             UL82§win = 'N';
037400101115             UL82§f7  = 'S';
037500101115             UL82§num = 5;
037600101115             UL82§att = 60;
037700101115             UL82§mss = msgjob;
037800101115         //?richiamo il pgm
037900101115             callp(e) trul82r (trul82ds);
038000101115         //?se il rcd è ancora allocato vado a fine lavoro e risottometto l'azione
038100101115             chain(e)  w_ATCrcd  TIATC00F;
038200101115             IF  %error;
038300101115               exsr Riprova;
038400101115             ENDIF;
038500101115           ENDIF;
038600101115         //?se il rcd non è più allocato aggiorno il commerciale
038700101115           ATCco3 = ccmn;
038800101115           update TIATC000;
038900101115         ENDDO;
039000101115         exec sql close F2;
039100101115
039200101115       //?TRATTATIVE fittizie e non
039300101115
039400101115       //?Aggiorno il commerciale apertura trattativa su TUTTE
039500101115         exec sql
039600101115           UPDATE  TIVIS00F  set VISCMMI = :ccmn
039700101115           where  VISCMMI = :ccmv;
039800101115
039900101115       //?Aggiorno il commerciale gestione trattativa su TUTTE le CHIUSE
040000101115         exec sql
040100101115           UPDATE  TIVIS00F  set VISCMM = :ccmn
040200101115           where  VISCMM = :ccmv and VISDCH > 0;
040300101115
040400101115       //?Aggiorno il commerciale gestione trattativa sulle APERTE
040500101115       //?solo se NON eiste già un'altra trattativa aperta su stesso cpo/ksc/cmm
040600101115       //?se la trattativa NON è fittizia
040700101115       //?per trattativa fittizia non controllo
040800101115         $EndF3 = *off;
040900101115         exec sql
041000101115           DECLARE  F3 cursor for
041100101115           select rrn(TIVIS00F) as w_VISrcd, TIVIS00F.* from TIVIS00F
041200101115           where  VISCMM = :ccmv  and  VISdch = 0
041300101115           order by  VISnrv;
041400101115
041500101115         exec sql
041600101115           open F3;
041700101115           IF sqlcode < 0;
041800101115             $EndF3 = *on;
041900101115           ENDIF;
042000101115
042100101115         DOW not $EndF3;
042200101115           exec sql
042300101115             fetch next from F3 into :w_VISrcd, :TIVISds;
042400101115           IF sqlcod = 100 or sqlcod < 0;
042500101115             $EndF3 = *on;
042600101115             leave;
042700101115           ENDIF;
042800101115
042900101115         //?Controllo che con il nuovo commerciale non esista già un'altra trattative
043000101115         //?CPO/KSC/CMM, se trattativa NON fittizia
043100101115           IF  w_VISffz = *blanks;
043200101115             clear tnta43ds;
043300101115             ITA43cpo = w_VIScpo;
043400101115             ITA43nrv = w_VISnrv;
043500101115             ITA43ksc = w_VISksc;
043600101115             ITA43cmm = ccmn;
043700101115             tnta43r (tnta43ds);
043800101115           //?trovo altra trattativa stampo segnalazione
043900101115             IF  OTA43nrv > 0;
044000101115               exsr stampa;
044100101115               iter;
044200101115             ENDIF;
044300101115           ENDIF;
044400101115
044500101115         //?Aggiorno il commerciale gestione trattativa
044600101115         //?gestisco allocazione rcd
044700101115           chain(e)  w_VISrcd  TIVIS00F;
044800101115           IF  %error;
044900101115             clear trul82ds;
045000101115             UL82§rrn = w_VISrcd;
045100101115             UL82§fil = 'TIVIS00F';
045200101115             UL82§win = 'N';
045300101115             UL82§f7  = 'S';
045400101115             UL82§num = 5;
045500101115             UL82§att = 60;
045600101115             UL82§mss = msgjob;
045700101115         //?richiamo il pgm
045800101115             callp(e) trul82r (trul82ds);
045900101115         //?se il rcd è ancora allocato vado a fine lavoro e risottometto l'azione
046000101115             chain(e)  w_VISrcd  TIVIS00F;
046100101115             IF  %error;
046200101115               exsr Riprova;
046300101115             ENDIF;
046400101115           ENDIF;
046500101115         //?se il rcd non è più allocato aggiorno il commerciale
046600101115           VIScmm = ccmn;
046700101115           update TIVIS000;
046800101115         //?Aggiorno anche le relative attività legate
046900101115           exsr Attivita;
047000101115         ENDDO;
047100101115         exec sql close F3;
047200101115
047300101115      /end-free
047400101005
047500100519     c
047600100519     c
047700041110     c     Fine          Tag
047800041110     c                   If        wbatch = *On
047900041110     c                   Eval      Kpjbu = Param
048000041110     c                   Eval      Kcoaz = 'MK45'
048100041110     c                   Call      'BCH10'
048200041110     c                   Parm                    KPJBA
048300041110     c                   EndIf
048400971125     C*
048500950828     C                   SETON                                        LR
048600101115
048700101115       //--------------------------------------------------------------
048800101115       //?Risottometto l'azione ed esco dal pgm.
048900101115       //--------------------------------------------------------------
049000101115     c     Riprova       begsr
049100101115
049200101115     c                   Eval      Kpjbu = Param
049300101115     c                   Eval      Kcoaz = 'MK45'
049400101115     c                   Call      'BCH10'
049500101115     c                   Parm                    KPJBA
049600101115
049700101115     c                   eval      *inlr = *on
049800101115     c                   return
049900101115
050000101115     c                   endsr
050100101115
050200101115       //--------------------------------------------------------------
050300101115       //?Stampo la segnalazione di errore.
050400101115       //--------------------------------------------------------------
050500101115     c     Stampa        begsr
050600101115
050700101115     c                   if        testata = *blanks
050800101115     c                   except    TESTA
050900101115     c                   eval      testata = 'S'
051000101115     c                   endif
051100101115
051200101115     c                   except    STAERR
051300101115
051400101115     c                   endsr
051500101115
051600101115      /free
051700101115       //--------------------------------------------------------------
051800101115       //?Aggiorno le attività legate alla trattativa aperta
051900101115       //--------------------------------------------------------------
052000101115       BEGSR Attivita;
052100101115
052200101115         exec sql
052300101115           UPDATE  TIATC00F  set ATCCMM = :ccmn
052400101115           where  ATCCMM = :ccmv  and  ATCnrv = :VISnrv;
052500101115
052600101115         exec sql
052700101115           UPDATE  TIATC00F  set ATCCCO = :ccmn
052800101115           where  ATCCCO = :ccmv  and  ATCnrv = :VISnrv;
052900101115
053000101115         exec sql
053100101115           UPDATE  TIATC00F  set ATCCO3 = :ccmn
053200101115           where  ATCCO3 = :ccmv  and  ATCnrv = :VISnrv;
053300101115
053400101115       ENDSR;
053500101115      /end-free
053600101115
053700950828     C*****************************************************************
053800950828     C     *INZSR        BEGSR
053900950828     C*
054000950825     C     *ENTRY        PLIST
054100950825     C                   PARM                    KPJBA
054200950825     C                   MOVEL     KPJBU         PARAM
054300040806
054400040806      * data del giorno
054500040806     c                   time                    w0140            14 0
054600040806      * UDATE IN GGMMAAAA
054700040806     c                   MOVE      W0140         WDTGIO            8 0
054800040806
054900040806      * UDATE IN AAAAMMGG
055000040806     c                   Z-ADD     WDTGIO        G08DAT
055100040806     c                   MOVEL     *BLANK        G08ERR
055200040806     c                   CALL      'XSRDA8'
055300040806     c                   PARM                    WLBDA8
055400040806     c                   MOVEL     G08INV        DATEU             8 0
055500020902      *
055600020902      * Reperisco dati job
055700020902     C                   EXSR      DatiJob
055800950829     C*
055900020902     C                   Z-ADD     1             CODUT             1 0
056000981204     C**
056100981204     C     SIMFEL        CHAIN     AZORG01L                           31
056200981204     C  N31              MOVEL     ORGFEL        WSFEL
056300981204     C   31              MOVEL     SIMFEL        WSFEL
056400981204     C     *LIKE         DEFINE    SIMFEL        WSFEL
056500950828     C*
056600950828     C                   MOVE      PACCMV        CCMV
056700950828     C                   MOVE      PACCMN        CCMN
056800950828     C*
056900950828     C     KCLP          KLIST
057000950829     C                   KFLD                    CODUT
057100950828     C                   KFLD                    KKCC
057200950828     C                   KFLD                    CCMV
057300950828     C*
057400950828     C     KACO          KLIST
057500950828     C                   KFLD                    CLPKUT
057600950828     C                   KFLD                    CLPKCC
057700950828     C                   KFLD                    CLPKSC
057800950828     C*
057900950829     C     KFCLP         KLIST
058000950830     C                   KFLD                    CODUT
058100950829     C                   KFLD                    KKCC
058200050927     C                   KFLD                    ccmv
058300950830     C*
058400950830     C     KSAC          KLIST
058500950830     C                   KFLD                    CCMN
058600950830     C                   KFLD                    SACAAA
058700950830     C                   KFLD                    SACMMM
058800950830     C                   KFLD                    SACAPL
058900950830     C                   KFLD                    SACTPV
059000950830     C                   KFLD                    SACCLV
059100981029     C                   KFLD                    SACFIE
059200981029     C                   KFLD                    SACFFZ
059300950830     C*
059400950830     C     KSAC1         KLIST
059500950830     C                   KFLD                    CCMV
059600950830     C                   KFLD                    SACAAA
059700950830     C                   KFLD                    SACMMM
059800950830     C                   KFLD                    SACAPL
059900950830     C                   KFLD                    SACTPV
060000950830     C                   KFLD                    SACCLV
060100981029     C                   KFLD                    SACFIE
060200981029     C                   KFLD                    SACFFZ
060300950828     C*
060400950828     C     *LIKE         DEFINE    CLPKCC        KKCC
060500950828     C     *LIKE         DEFINE    CPOCMM        CCMV
060600950828     C     *LIKE         DEFINE    CPOCMM        CCMN
060700950828     C*
060800950828     C                   ENDSR
060900020902      *----------------------------------------------------------------
061000020902      *  Reperisco Dati del job (Utente/Operativi)
061100020902      *----------------------------------------------------------------
061200020902     C     DatiJob       BegSr
061300020902      *
061400020902     C     *dtaara       define    §azute        azuteds
061500020902     C     *dtaara       define    §datiute      ddatiute
061600020902      *
061700020902     C                   in(E)     *dtaara
061800020902     C                   IF        %Error or RSUT = *blanks
061900020902     C                   call      'TIBS34R'
062000020902     C                   parm                    Tibs34Ds
062100020902     C                   in        *dtaara
062200020902     C                   ENDIF
062300020902     C                   endsr
062400020902     C*
062500020902     C*---------------------------------------------------------------*
062600041110     OTNCPO00   E            AGGCPO
062700950828     O                       CPOCMM
062800950830     O                       CPODTR
062900050201     OCNCLP00   E            AGGCLP
063000950828     O                       CLPAGE
063100950828     OCNACO000  E            AGGACO
063200950828     O                       ACOFTR
063300950830     O                       ACODTR
063400950828     OTFSAC000  E            AGGSAC
063500950828     O                       SACCMM
063600041011     O                       SACDTR
063700050927     OTFCLP000  E            AGFCLP
063800950828     O                       CLPAGE
063900100519     O
064000101115
064100100520     OQSYSPRT   E            TESTA            02 04
064200100520     O                                        +  20 '*** CAMBIO CODICE '
064300101115     O                                        +   1 'COMMERCIALE da '
064400100519     O                       CCMV             +   1
064500100520     O                                        +   1 'a '
064600100519     O                       CCMN             +   1
064700100520     O                                        +   1 '***'
064800100519     O                       *date              118 '  /  /    '
064900101115     O          E            staERR      2
065000100519     O                                              'TRATTATIVA '
065100101115     O                       w_VISnrv      Z   +  1
065200101116     O                                         +  1 'non aggiornata: già '
065300101116     O                                         +  1 'presente la trattativa '
065400101115     O                       OTA43nrv      Z   +  1
065500101116     O                                         +  1 'su stesso potenziale -'
065600100519     O                                         +  1 'cliente'
