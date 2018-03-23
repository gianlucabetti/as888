000100990908     H dftactgrp(*yes)
000200000313     F*
000300030811     Ffnacr01l  if   e           k disk
000400090618     Ftntbe01l  if   e           k disk
000500030811     FFNORM00F  O    E             DISK
000600030811     FFNORF00F  O    E             DISK
000700070709     ffnorg00f  o    e             disk
000800030811     FTIORI00F  O    E             DISK
000900050823     FTIVGD00F  O    E             DISK
001000000724     D*------------
001100030811     D* DS ESTERNE
001200030811     D*------------
001300040108     D tisi95ds      e ds                  inz
001400030811     D trul33ds      e ds                  inz
001500030811     D trul13ds      e ds                  inz
001600110406     D tibs02ds      e ds                  inz
001700030811     D dfar          e ds                  inz
001800030811     D dorm01        e ds                  inz
001900110406     D dpvo          e ds                  inz
002000000313     D*
002100040108     D kpjba         e ds                  inz
002200040108     D fivabds       e ds                  extname(fivab00f)
002300040108     D tivlrds       e ds                  extname(tivlr00f)
002400050823     D fnvapds       e ds                  extname(fnvap00f)
002500000313     D*
002600040108     D InTipl          s              1
002700030811     D Ktbcod          s                   LIKE(tbeCOD)
002800030811     D Ktbke1          s                   LIKE(tbeKE1)
002900031229     D wNSR            s              2
003000031229     D wCRC            s             10
003100041213     D wCRCAcr         s             10  0
003200031229     D wKSC            s              7
003300031229     D wCTR            s                   LIKE(ormCTR)
003400090914     d wpag            s                   like(ormpag)
003500090914     d wpor            s                   like(ormpor)
003600051201     D wPOC            s              3
003700070305     D wCOLPES         s              1
003800070709     d wvlm            s             10  3
003900090618     d §numori         s              7  0 inz
004000050907     D*
004100050907     D TxtInOut        S           2048
004200050907     D ElencoChar      S            256
004300050907     D TipoElenco      S              1
004400050907     D CharSost        S              1
004500050907     D UpperCase       S              1
004600050907     D ChkNull         S              1
004700050907     D CharNull        S              1
004800050907     D EsitoBon        S              1
004900010606     D*------------------
005000010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005100010606     D*------------------
005200010606     D WLBDA8          DS                  INZ
005300010606     D  G08DAT                 1      8  0
005400010606     D  G08INV                 9     16  0
005500010606     D  G08ERR                17     17
005600010606     D  G08TGI                18     22  0
005700060113     D*------------------
005800060113     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
005900060113     D*------------------
006000060113     D TISIT0DS      E DS
006100060113     D DDSTFLO       E DS
006200030811     D*------------------
006300030811     D* DS RIDEFINIZIONE CHIAVE BOLLA ALFA
006400030811     D*------------------
006500030811     D                 DS                  INZ
006600040108     D  wvabAAS                1      4  0
006700120327     D  wvabFGS                5      7  0
006800040108     D  wvabNRS                8      9  0
006900040108     D  wvabNSP               10     16  0
007000030811     D  vabKEY                 1     16
007100010606     D*
007200060306     D Cli_Abil        s              1    inz(*off)
007300060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
007400010606
007500010606
007600010606
007700010606
007800040108     C*
007900040108     C* Se tipo lancio diverso da "chiusura" => eseguo solo elaborazione
008000040108     C                   if        InTipl <> 'C'
008100050907     C                   exsr      exebon
008200040108     C                   exsr      opeini
008300040108     C                   exsr      exekeyvab
008400040108     C                   exsr      exeorm
008500040108     C                   seton                                        rt
008600040108     C                   endif
008700040108     C*
008800040108     C* Se tipo lancio "chiusura" => eseguo solo chiusura
008900040108     C                   if        InTipl = 'C'
009000030811     C                   exsr      exefin
009100040108     C                   seton                                        lr
009200040108     C                   endif
009300010601     C*
009400040108
009500040108
009600040108
009700040108
009800040108      /TITLE Operazioni iniziali e impostazoini d default
009900040108     C     opeini        BEGSR
010000040108     C*
010100040108     C* Verifico se impostato il parametro relativo al numero serie orm (ORMNSR)
010200040108     C                   if        %subst(vlrppt:4:2) <> *blanks
010300040108     C                   eval      wNSR = %subst(vlrppt:4:2)
010400040108     C                   endif
010500040108     C*
010600040108     C* Verifico se impostato il parametro relativo al codice consegna (ORMCRC)
010700040108     C                   if        %subst(vlrppt:6:10) <> *blanks
010800040227     C                   eval      wCRC = %subst(vlrppt:6:10)
010900040227     C                   else
011000040227     C                   eval      wCRC = %editw(VABCCM:'       ') + '444'
011100040108     C                   endif
011200040108     C*
011300040108     C* Verifico se impostato il parametro relativo al codice pagante (ORMKSC)
011400040108     C                   if        %subst(vlrppt:16:7) <> *blanks
011500040227     C                   eval      wKSC = %subst(vlrppt:16:7)
011600040227     C                   else
011700041213     C* Se codice cliente pagante ORM nn è passato nei parametri lo reperisco dall'anagrafica
011800041213     C* clienti d ritiro agganciandolo tramite codice cliente destinatario (codice consegna ORM)
011900041213     C                   movel(p)  wCRC          wCRCAcr
012000041213     C                   movel     *blanks       wCTR
012100041213     C     wCRCAcr       chain     fnacr01l
012200041213     C                   if        %found(fnacr01l)
012300090513     c                   if        acrksc > 0
012400090513     C                   movel(p)  acrksc        wKSC
012500090513     c                   else
012600090513     C                   movel(p)  acrCRO        wKSC
012700090513     c                   endif
012800041213     C* Da qui reperisco anche il relativo codice tariffa
012900041213     C                   if        acrCCC <> 999
013000041213     C                   move(p)   acrCCC        wCTR
013100041213     C                   endif
013200041213     C                   endif
013300040108     C                   endif
013400040108     C*
013500040108     C* Verifico se impostato il parametro relativo al codice pagante (ORMCTR)
013600040108     C                   if        %subst(vlrppt:23:3) <> *blanks
013700040108     C                   eval      wCTR = %subst(vlrppt:23:3)
013800040108     C                   endif
013900090914     C*
014000090914     C* Verifico se impostato il parametro relativo al chi paga (ORMPAG)
014100090914     C                   if        %subst(vlrppt:36:1) <> *blanks
014200090914     C                   eval      wpag = %subst(vlrppt:36:1)
014300090914     c                   else
014400090914     C******
014500090914      * ATTENZIONE: se si modifica il DEFAULT del flag "chi paga" occorre gestire anche il
014600090914      * reperimento del codice cliente dall'anagrafica clienti ritiro (FNACR01L)
014700090914      * nelle specifiche precedenti dove imposta wksc
014800090914     C******
014900090914     C                   eval      wpag = 'D'
015000090914     C                   endif
015100051201     C*
015200051201     C* Verifico se impostato il parametro relativo al P.O. consegna (ORMPOC)
015300051201     C                   movel     *blanks       wPOC
015400080418     C                   if        %subst(vlrppt:32:3) <> *blanks
015500080418     C                   eval      wPOC = %subst(vlrppt:32:3)
015600051201     C                   endif
015700070305     C*
015800070305     C* Verifico se richiesta valorizzazione colli e peso con quelli della bolla originale "RC"
015900070305     C                   movel     *blanks       wCOLPES
016000080418     C                   if        %subst(vlrppt:35:1) = 'S'
016100070305     C                   eval      wCOLPES = 'S'
016200070305     C                   endif
016300090914     C*
016400090914     C* Verifico se impostato il parametro relativo alla filiale ritiro (ORMPOR)
016500090914     C                   if        %subst(vlrppt:37:3) <> *blanks
016600090914     C                   eval      wpor = %dec(%subst(vlrppt:37:3):3:0)
016700090914     c                   else
016800090914     c                   clear                   wpor
016900090914     c                   endif
017000040108     C*
017100040108     C                   ENDSR
017200040108     C***
017300040108
017400040108
017500040108
017600040108
017700040108      /TITLE Costruzione chiave bolla
017800040108     C     exekeyvab     BEGSR
017900040108     C*
018000040108     C                   eval      wvabAAS = vabAAS
018100120327     C                   eval      wvabFGS = vabFGS
018200040108     C                   eval      wvabNRS = vabNRS
018300040108     C                   eval      wvabNSP = vabNSP
018400040108     C*
018500040108     C                   ENDSR
018600040108     C***
018700050907
018800050907
018900050907
019000050907
019100050907      /TITLE "Bonifica" dati bolla da caratteri nn voluti (tramite XCHKCHAR)
019200050907     C     exebon        BEGSR
019300050907     C*
019400050907     C* Imposto i parametri x il *pgm d "bonifica"
019500050907     C                   EVAL      ElencoChar = *blanks
019600050907     C                   EVAL      TipoElenco = *blanks
019700050907     C                   EVAL      CharSost   = *blanks
019800050907     C                   EVAL      UpperCase  = '1'
019900050907     C                   EVAL      ChkNull    = '1'
020000050907     C                   EVAL      CharNull   = *blanks
020100050907     C*
020200050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
020300050907     C                   EVAL      TxtInOut   = VABRSD
020400050907     C                   EVAL      EsitoBon   = *blanks
020500050907     C*
020600050907     C                   CALL      'XCHKCHAR'
020700050907     C                   PARM                    TxtInOut
020800050907     C                   PARM                    ElencoChar
020900050907     C                   PARM                    TipoElenco
021000050907     C                   PARM                    CharSost
021100050907     C                   PARM                    UpperCase
021200050907     C                   PARM                    ChkNull
021300050907     C                   PARM                    CharNull
021400050907     C                   PARM                    EsitoBon
021500050907     C*
021600050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
021700050907     C                   IF        EsitoBon = '1'
021800050907     C                   EVAL      VABRSD = TxtInOut
021900050907     C                   ENDIF
022000050907     C*
022100050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
022200050907     C                   EVAL      TxtInOut   = VABIND
022300050907     C                   EVAL      EsitoBon   = *blanks
022400050907     C*
022500050907     C                   CALL      'XCHKCHAR'
022600050907     C                   PARM                    TxtInOut
022700050907     C                   PARM                    ElencoChar
022800050907     C                   PARM                    TipoElenco
022900050907     C                   PARM                    CharSost
023000050907     C                   PARM                    UpperCase
023100050907     C                   PARM                    ChkNull
023200050907     C                   PARM                    CharNull
023300050907     C                   PARM                    EsitoBon
023400050907     C*
023500050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
023600050907     C                   IF        EsitoBon = '1'
023700050907     C                   EVAL      VABIND = TxtInOut
023800050907     C                   ENDIF
023900050907     C*
024000050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
024100050907     C                   EVAL      TxtInOut   = VABLOD
024200050907     C                   EVAL      EsitoBon   = *blanks
024300050907     C*
024400050907     C                   CALL      'XCHKCHAR'
024500050907     C                   PARM                    TxtInOut
024600050907     C                   PARM                    ElencoChar
024700050907     C                   PARM                    TipoElenco
024800050907     C                   PARM                    CharSost
024900050907     C                   PARM                    UpperCase
025000050907     C                   PARM                    ChkNull
025100050907     C                   PARM                    CharNull
025200050907     C                   PARM                    EsitoBon
025300050907     C*
025400050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
025500050907     C                   IF        EsitoBon = '1'
025600050907     C                   EVAL      VABLOD = TxtInOut
025700050907     C                   ENDIF
025800050907     C*
025900050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABPRD
026000050907     C                   EVAL      TxtInOut   = VABPRD
026100050907     C                   EVAL      EsitoBon   = *blanks
026200050907     C                   EVAL      UpperCase  = *blanks
026300050907     C*
026400050907     C                   CALL      'XCHKCHAR'
026500050907     C                   PARM                    TxtInOut
026600050907     C                   PARM                    ElencoChar
026700050907     C                   PARM                    TipoElenco
026800050907     C                   PARM                    CharSost
026900050907     C                   PARM                    UpperCase
027000050907     C                   PARM                    ChkNull
027100050907     C                   PARM                    CharNull
027200050907     C                   PARM                    EsitoBon
027300050907     C*
027400050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
027500050907     C                   IF        EsitoBon = '1'
027600050907     C                   EVAL      VABPRD = TxtInOut
027700050907     C                   ENDIF
027800050907     C*
027900050907     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNZD
028000050907     C                   EVAL      TxtInOut   = VABNZD
028100050907     C                   EVAL      EsitoBon   = *blanks
028200050907     C                   EVAL      UpperCase  = *blanks
028300050907     C*
028400050907     C                   CALL      'XCHKCHAR'
028500050907     C                   PARM                    TxtInOut
028600050907     C                   PARM                    ElencoChar
028700050907     C                   PARM                    TipoElenco
028800050907     C                   PARM                    CharSost
028900050907     C                   PARM                    UpperCase
029000050907     C                   PARM                    ChkNull
029100050907     C                   PARM                    CharNull
029200050907     C                   PARM                    EsitoBon
029300050907     C*
029400050907     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
029500050907     C                   IF        EsitoBon = '1'
029600050907     C                   EVAL      VABNZD = TxtInOut
029700050907     C                   ENDIF
029800050907     C*
029900050907     C                   ENDSR
030000050907     C***
030100010601
030200010601
030300010601
030400010601
030500010601     C*--------------------------------------------------------
030600040108     C* EXEORM - OPERAZIONI DI SCRITTURA VARI FILE ORM        *
030700010601     C*--------------------------------------------------------
030800040108     C     EXEORM        BEGSR
030900030811     C*
031000030811     C                   exsr      wriORM
031100030811     C                   exsr      wriORF
031200070709     C                   exsr      wriORG
031300030811     C                   exsr      wriORI
031400050823     C                   exsr      wriVGD
031500040108     C*
031600910830     C                   ENDSR
031700000613     C***
031800030811
031900030811
032000030811
032100030811      /TITLE Scrittura record in FNORM00F
032200030811     C     wriORM        BEGSR
032300030811     C*
032400030811     C                   clear                   FNORM000
032500030811     C* Valorizzo buffer record
032600030811     C                   eval      ormATB = *blanks
032700120327     C                   eval      ormPOE = vabFGS
032800031229     C                   move      wNSR          ormNSR
032900030811     C* Stacco un numeratore da AZNUM
033000030811     C                   clear                   TRUL33DS
033100030811     C                   eval      I33OPE = *zeros
033200030811     C                   eval      I33PO1 = 046
033300030811     C                   eval      I33CNU = 300
033400030811     C                   eval      I33NUM = 1
033500030811     C                   movel     TRUL33DS      KPJBU
033600030811     C                   call      'TRUL33R'
033700030811     C                   parm                    KPJBA
033800030811     C                   movel     KPJBU         TRUL33DS
033900030811     C                   if        O33ERR = *zeros
034000030811     C                   eval      ormNOR = O33NRF
034100030811     C                   else
034200030811     C                   eval      ormNOR = *all'9'
034300030811     C                   endif
034400030811     C*
034500030811     C                   eval      ormNRV = *zeros
034600030811     C                   eval      ormTOR = 'S'
034700030811     C                   eval      ormTCO = 'F'
034800030811     C                   eval      ormDAO = datcor
034900030811     C                   eval      ormOAO = oracor
035000030811     C                   eval      ormCOR = vabCCM*1000+333
035100030811     C* Reperisco i dati anagrafici (dall'anagrafica ritiri) del codice ORDINANTE
035200030811     C     ormCOR        chain     fnacr01l
035300030811     C                   if        %found(fnacr01l)
035400030811     C                   eval      ormRSO = acrRSR
035500030811     C                   eval      ormINO = acrINR
035600030811     C                   eval      ormCAO = acrCAR
035700030811     C                   eval      ormLOO = acrLOR
035800030811     C                   eval      ormPRO = acrPRR
035900030811     C                   eval      ormNAO = acrNAR
036000030811     C                   eval      ormNAM = acrNTM
036100030811     C                   else
036200030811     C                   eval      ormRSO = *all'*'
036300030811     C                   eval      ormINO = *all'*'
036400030811     C                   eval      ormCAO = *all'0'
036500030811     C                   eval      ormLOO = *all'*'
036600030811     C                   eval      ormPRO = *all'*'
036700030811     C                   eval      ormNAO = *all'*'
036800030811     C                   eval      ormNAM = *all'*'
036900030811     C                   endif
037000030811     C                   eval      ormCRA = *zeros
037100030811     C                   eval      ormRSR = vabRSD
037200030811     C                   eval      ormINR = vabIND
037300030811     C                   eval      ormCAR = vabCAD
037400030811     C                   eval      ormLOR = vabLOD
037500030811     C                   eval      ormPRR = vabPRD
037600030811     C                   eval      ormNAR = vabNZD
037700030909     C                   eval      ormRER = 'ORM CONTESTUALE CONSEGNA'
037800030909     C                   eval      ormTER = 'ORM CONT. CONS.'
037900031229     C                   move      wCRC          ormCRC
038000030811     C* Reperisco i dati anagrafici (dall'anagrafica ritiri) del codice CONSEGNA
038100030811     C     ormCRC        chain     fnacr01l
038200030811     C                   if        %found(fnacr01l)
038300030811     C                   eval      ormRSC = acrRSR
038400030811     C                   eval      ormINC = acrINR
038500030811     C                   eval      ormCAC = acrCAR
038600030811     C                   eval      ormLOC = acrLOR
038700030811     C                   eval      ormPRC = acrPRR
038800030811     C                   eval      ormNAC = acrNAR
038900030811     C                   else
039000030811     C                   eval      ormRSC = *all'*'
039100030811     C                   eval      ormINC = *all'*'
039200030811     C                   eval      ormCAC = *all'0'
039300030811     C                   eval      ormLOC = *all'*'
039400030811     C                   eval      ormPRC = *all'*'
039500030811     C                   eval      ormNAC = *all'*'
039600030811     C                   endif
039700030811     C                   eval      ormFFD = 'N'
039800030811     C* Calcolo la data ritiro richiesta aggiungendo alla data aperura ORM 1 gg. lavorativo
039900030811     C* (tramite TRUL13R)
040000030811     C                   z-add     1100          wOra              4 0
040100030811     C                   clear                   TRUL13DS
040200030811     C                   eval      I13MOD = 'P'
040300030811     C                   eval      I13FIL = ormPOE
040400030811     C                   eval      I13DIN = ormDAO
040500030811     C                   eval      I13HIN = wOra
040600030811     C                   eval      I13GIO = 1
040700030811     C                   eval      I13ORE = *zeros
040800030811     C                   call      'TRUL13R'
040900030811     C                   parm                    TRUL13DS
041000030811     C                   if        O13ERR = *blanks
041100030811     C                   eval      ormDAR = O13DFI
041200030811     C                   else
041300030811     C                   eval      ormDAR = ormDAO
041400030811     C                   endif
041500030811     C*
041600030811     C                   eval      ormORR = wORA
041700030811     C                   eval      ormRMP = *blanks
041800070305     C*
041900070305     C                   if        wCOLPES = 'S'
042000070305     C                   eval      ormNCL = vabNCL
042100070305     C                   eval      ormPKG = vabPKB
042200070305     C                   else
042300031229     C                   eval      ormNCL = 1
042400030811     C                   eval      ormPKG = 1
042500070305     C                   endif
042600030811     C                   eval      ormVLM = *zeros
042700030811     C                   eval      ormBNC = *zeros
042800030811     C                   eval      ormBLC = *zeros
042900030811     C                   eval      ormATT = *zeros
043000030811     C                   eval      ormMTC = *zeros
043100090914
043200090914     C                   move      wpag          ormpag
043300041213     C******
043400031229     C                   move      wKSC          ormKSC
043500031229     C                   movel     wCTR          ormCTR
043600040305     C***                eval      ormPOR = vabLNA
043700040305     C                   eval      ormPOR = *zeros
043800090914     c                   eval      ormpor = wpor
043900030811     C                   movel     vabZNC        ormZOR
044000030811     C* Reperisco P.O. Consegna tramite TISI95R richiamato con i dati instradamento del destinatario
044100030811     C                   clear                   TISI95DS
044200030811     C                   eval      I95TCN = '3'
044300030811     C                   eval      I95NAR = ormNAC
044400030811     C                   eval      I95CAP = ormCAC
044500030811     C                   eval      I95LOC = ormLOC
044600030811     C                   eval      I95PRV = ormPRC
044700030811     C                   eval      I95LKG = ormPKG
044800030811     C                   call      'TISI95R'
044900030811     C                   parm                    TISI95DS
045000030811     C                   if        O95ERR = *blanks
045100030811     C                   eval      ormPOC = O95LNA
045200030811     C                   else
045300030811     C                   eval      ormPOC = *zeros
045400030811     C                   endif
045500060113     C*
045600060113     C* Eseguo controllo x eventuale destinatario presente in anagrafiuco dest. particolari
045700060113     C* x forzatura linea/zona instradamento prestabilito
045800060113     C                   clear                   tisit0ds
045900060113     C                   movel     'E'           it0TLA
046000060113     C                   movel     ormNAC        it0NAZ
046100060113     C                   movel     ormPRC        it0PRV
046200060113     C                   movel     ormCAC        it0CAP
046300060113     C                   movel     ormLOC        it0LOC
046400060113     C                   movel     ormINC        it0IND
046500060113     C                   movel     ormRSC        it0RAG
046600060113     C                   call      'TISIT5R'
046700060113     C                   parm                    tisit0ds
046800060113     C*
046900060113     C* Se trovato destinatario tra i dest. particolari verifico se presenti linea/zona
047000060113     C* instradamento prestabiliti
047100060113     C                   if        ot0ERR = *blanks
047200060113     C                   eval      DDSTFLO = ot0FLO
047300060113     C                   if        §DSTLNA <> *blanks
047400060113     C                   move(p)   §DSTLNA       ormPOC
047500060113     C                   endif
047600060113     C                   endif
047700051201     C*
047800051201     C* Gestisco eventuale forzatura proveniente da anagrafica clienti/tipifile
047900051201     C                   if        wPOC <> *blanks
048000051201     C                   move(p)   wPOC          ormPOC
048100051201     C                   endif
048200030811     C*
048300030811     C                   eval      ormDDT = 'N'
048400030811     C                   eval      ormCST = *zeros
048500030811     C                   eval      ormVCS = *blanks
048600030811     C                   eval      ormFCS = *blanks
048700030811     C                   eval      ormDFO = datcor
048800030811     C                   eval      ormOFO = oracor
048900030811     C                   eval      ormFAO = 100
049000030811     C                   eval      ormSTO = *zeros
049100030811     C                   eval      ormDST = datcor
049200030811     C                   eval      ormNPG = *zeros
049300030811     C                   eval      ormNDC = *zeros
049400030811     C                   eval      ormDDC = *zeros
049500030811     C                   eval      ormSTP = *zeros
049600030811     C                   eval      ormPOS = *zeros
049700030811     C                   eval      ormORS = *zeros
049800030811     C                   eval      ormRFA = 'B' + %subst(vabKEY:3:14)
049900030811     C                   eval      ormTAP = *blanks
050000070709     C**!!!              eval      ormETI = *blanks
050100030811     C                   eval      ormSPI = *blanks
050200070709
050300070709      * imposto il campo ORMETI (falg orm in fase assegnabile) in base alla fase
050400070709      * impostata in precedenza
050500070709     c                   movel(p)  'FAR'         KtbCOD
050600070709     c                   movel(p)  ormfao        KtbKE1
050700070709     c     KEYtbe        chain     tntbe01l
050800070709     c                   if        %found(tntbe01l)
050900070709     c                   movel(P)  tbeuni        DFAR
051000070709     c                   else
051100070709     c                   clear                   DFAR
051200070709     c                   endif
051300070709     c                   eval      ormeti = d§farass
051400070709
051500030811     C* Valorizzo il campo dei FLag Operativi secondo la DS DORM01
051600030811     C                   clear                   DORM01
051700030811     C                   eval      §ormCB = *blanks
051800030811     C                   eval      §ormFR = *blanks
051900030811     C                   eval      §ormKS = 'S'
052000030811     C                   eval      §ormFD = 'S'
052100030811     C                   eval      §ormIC = *blanks
052200030811     C                   eval      §orCOM = *blanks
052300030811     C                   eval      §orPFB = 'A'
052400030811     C                   eval      §ormXX = *blanks
052500090618     C* stacco un numeratore da AZNUM
052600090618     C                   clear                   trul33ds
052700090618     C                   eval      I33OPE = *zeros
052800090618     C                   eval      I33CNU = 600
052900090618     C                   eval      I33NUM = 1
053000090618     C                   movel     TRUL33DS      KPJBU
053100090618     C                   call      'TRUL33R'
053200090618     C                   parm                    KPJBA
053300090618     C                   movel     KPJBU         TRUL33DS
053400090618     C                   if        O33ERR = *zeros
053500090618     C                   z-add     O33NRF        §numori
053600090618     c                   else
053700090618     C                   eval      §numori = *all'9'
053800090618     C                   endif
053900030811     C                   movel     §numori       §ormPG
054000030811     C*
054100110406     C* Verifico eventuali "personalizzazioni" previste x il codice cliente pagante
054200110406     C                   clear                   dpvo
054300110406     C                   clear                   tibs02ds
054400110406     C                   eval      t02mod = 'C'
054500110406     C                   eval      t02cod = 'PVO'
054600110406     C                   eval      t02ke1 = %editc(ormKSC:'X')
054700110406     C                   eval      t02ke2 = 'RCK'
054800110406     C                   call      'TIBS02R'
054900110406     C                   parm                    kpjba
055000110406     C                   parm                    tibs02ds
055100110406     C                   if        t02err = *blanks
055200110406     C                   eval      dpvo = t02uni
055300110406     C
055400110406     C                   if        d§pvocb  <> *blanks
055500110406     C                   eval      §ormcb = d§pvocb
055600110406     C                   endif
055700110406     C                   if        d§pvofr  <> *blanks
055800110406     C                   eval      §ormfr = d§pvofr
055900110406     C                   endif
056000110406     C                   if        d§pvoks  <> *blanks
056100110406     C                   eval      §ormks = d§pvoks
056200110406     C                   endif
056300110406     C                   if        d§pvofd  <> *blanks
056400110406     C                   eval      §ormfd = d§pvofd
056500110406     C                   endif
056600110406     C                   if        d§pvoic  <> *blanks
056700110406     C                   eval      §ormic = d§pvoic
056800110406     C                   endif
056900110406     C                   if        d§pvosrm <> *blanks
057000110406     C                   eval      §orsrm = d§pvosrm
057100110406     C                   endif
057200110406     C                   endif
057300110406     C*
057400110406     C                   eval      ormFLO = DORM01
057500040910     C                   eval      ormDTT = datcor
057600030811     C*
057700030811     C* Scarico il buffer su file
057800030811     C                   write     FNORM000
057900030811     C*
058000030811     C                   ENDSR
058100030811     C***
058200030811
058300030811
058400030811
058500030811      /TITLE Scrittura record in FNORF00F
058600030811     C     wriORF        BEGSR
058700030811     C*
058800030811     C                   clear                   FNORF000
058900030811     C* Valorizzo buffer record
059000030811     C                   eval      orfATB = *blanks
059100030811     C                   eval      orfPOE = ormPOE
059200030811     C                   eval      orfNSR = ormNSR
059300030811     C                   eval      orfNOR = ormNOR
059400030811     C                   eval      orfNRV = ormNRV
059500030811     C                   eval      orfPOG = ormPOE
059600030811     C                   eval      orfDAE = datcor
059700030811     C                   eval      orfORE = oracor
059800030811     C                   eval      orfFAR = ormFAO
059900030811     C                   eval      orfCAR = *blanks
060000030811     C                   eval      orfPUE = 'BATCH'
060100030811     C*
060200030811     C* Scarico il buffer su file
060300030811     C                   write     FNORF000
060400030811     C*
060500030811     C                   ENDSR
060600030811     C***
060700070709
060800070709
060900070709
061000070709      /TITLE Scrittura record in FNORG00F
061100070709     C     wriORG        BEGSR
061200070709     C*
061300070709     C                   clear                   FNORG000
061400070709     C* Valorizzo buffer record
061500070709     C                   eval      orgATB = *blanks
061600070709     C                   eval      orgPOE = ormPOE
061700070709     C                   eval      orgNSR = ormNSR
061800070709     C                   eval      orgNOR = ormNOR
061900070709     C                   eval      orgNRV = ormNRV
062000070709     C                   eval      orgPOR = ormPOR
062100070709      * imposto il peso = a quello dell'ORM
062200070709     c                   eval      orgPKG = ormPKG
062300070709      * imposto il volume in base al rapporto peso/volume
062400070709     c                   eval(h)   wvlm = ormPKG / 200
062500070709      * se troppo alto il volume lo imposto al massimo
062600070709     c                   if        wvlm > 99,999
062700070709     c                   eval      wvlm = 99,990
062800070709     c                   endif
062900070709     c                   eval      orgvlm = wvlm
063000070709     C*
063100070709     C* Scarico il buffer su file
063200070709     C                   write     FNORG000
063300070709     C*
063400070709     C                   ENDSR
063500030811
063600030811
063700030811
063800030811      /TITLE Scrittura record in TIORI00F
063900030811     C     wriORI        BEGSR
064000030811     C*
064100030811     C                   clear                   TIORI000
064200030811     C* Valorizzo buffer record
064300030811     C                   movel     §ormPG        oriPRG
064400030811     C                   eval      oriDER = datcor
064500030811     C                   eval      oriOER = oracor
064600030811     C                   eval      oriIDC = vlrKSC
064700030811     C                   eval      oriPOE = ormPOE
064800030811     C                   eval      oriNSR = ormNSR
064900030811     C                   eval      oriNOR = ormNOR
065000030811     C                   eval      oriNRV = ormNRV
065100030811     C                   eval      oriFLO = *blanks
065200030811     C                   eval      vaoATB = ormATB
065300030811     C                   eval      vaoPOE = ormPOE
065400030811     C                   eval      vaoNSR = ormNSR
065500030811     C                   eval      vaoNOR = ormNOR
065600030811     C                   eval      vaoNRV = ormNRV
065700030811     C                   eval      vaoTOR = ormTOR
065800030811     C                   eval      vaoTCO = ormTCO
065900030811     C                   eval      vaoDAO = ormDAO
066000030811     C                   eval      vaoOAO = ormOAO
066100030811     C                   eval      vaoCOR = ormCOR
066200030811     C                   eval      vaoRSO = ormRSO
066300030811     C                   eval      vaoINO = ormINO
066400030811     C                   eval      vaoCAO = ormCAO
066500030811     C                   eval      vaoLOO = ormLOO
066600030811     C                   eval      vaoPRO = ormPRO
066700030811     C                   eval      vaoNAO = ormNAO
066800030811     C                   eval      vaoCRA = ormCRA
066900030811     C                   eval      vaoRSR = ormRSR
067000030811     C                   eval      vaoINR = ormINR
067100030811     C                   eval      vaoCAR = ormCAR
067200030811     C                   eval      vaoLOR = ormLOR
067300030811     C                   eval      vaoPRR = ormPRR
067400030811     C                   eval      vaoNAR = ormNAR
067500030811     C                   eval      vaoRER = ormRER
067600030811     C                   eval      vaoTER = ormTER
067700030811     C                   eval      vaoCRC = ormCRC
067800030811     C                   eval      vaoRSC = ormRSC
067900030811     C                   eval      vaoINC = ormINC
068000030811     C                   eval      vaoCAC = ormCAC
068100030811     C                   eval      vaoLOC = ormLOC
068200030811     C                   eval      vaoPRC = ormPRC
068300030811     C                   eval      vaoNAC = ormNAC
068400030811     C                   eval      vaoFFD = ormFFD
068500030811     C                   eval      vaoDAR = ormDAR
068600030811     C                   eval      vaoORR = ormORR
068700030811     C                   eval      vaoRMP = ormRMP
068800030811     C                   eval      vaoNAM = ormNAM
068900030811     C                   eval      vaoNCL = ormNCL
069000030811     C                   eval      vaoPKG = ormPKG
069100030811     C                   eval      vaoVLM = ormVLM
069200030811     C                   eval      vaoBNC = ormBNC
069300030811     C                   eval      vaoBLC = ormBLC
069400030811     C                   eval      vaoATT = ormATT
069500030811     C                   eval      vaoMTC = ormMTC
069600030811     C                   eval      vaoPAG = ormPAG
069700030811     C                   eval      vaoKSC = ormKSC
069800030811     C                   eval      vaoCTR = ormCTR
069900030811     C                   eval      vaoPOR = ormPOR
070000030811     C                   eval      vaoZOR = ormZOR
070100030811     C                   eval      vaoPOC = ormPOC
070200030811     C                   eval      vaoNO1 = ormNO1
070300030811     C                   eval      vaoNO2 = ormNO2
070400030811     C                   eval      vaoDDT = ormDDT
070500030811     C                   eval      vaoRFA = ormRFA
070600030811     C                   eval      vaoSTO = ormSTO
070700030811     C                   eval      vaoSPI = ormSPI
070800030811     C                   eval      vaoFLO = ormFLO
070900030811     C*
071000030811     C* Scarico il buffer su file
071100030811     C                   write     TIORI000
071200030811     C*
071300030811     C                   ENDSR
071400030811     C***
071500030811
071600030811
071700030811
071800050823      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
071900050823     C     wriVGD        BEGSR
072000030811     C*
072100030811     C* Reperisco la descrizione della fase da tabella
072200030811     C                   movel(p)  'FAR'         KtbCOD
072300030811     C                   movel(p)  '000'         KtbKE1
072400030811     C     KEYtbe        chain     tntbe01l
072500030811     C                   if        %found(tntbe01l)
072600030811     C                   movel(P)  tbeuni        DFAR
072700030811     C                   else
072800030811     C                   clear                   DFAR
072900030811     C                   endif
073000030811     C*
073100030811     C* Valorizzo buffer record
073200050823     C                   clear                   FNVAPDS
073300030811     C                   eval      vapIDC = oriIDC
073400030811     C                   eval      vapPOE = oriPOE
073500030811     C                   eval      vapRFA = vaoRFA
073600030811     C                   eval      vapPOG = oriPOE
073700030811     C                   eval      vapDAE = oriDER
073800030811     C                   eval      vapORE = oriOER
073900030811     C                   if        d§fardva <> *blanks
074000030811     C                   eval      vapDFA = d§fardva
074100030811     C                   else
074200030811     C                   eval      vapDFA = d§fardes
074300030811     C                   endif
074400050823     C*
074500050823     C                   clear                   tivgd000
074600080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
074700050823     C                   eval      vgdTIP = 'VP'
074800050823     C                   eval      vgdKSU = vapIDC
074900050823     C                   eval      vgdTSC = 'WW'
075000050823     C                   eval      vgdDAT = datcor
075100050823     C                   eval      vgdPGM = 'TITV84R'
075200060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
075300060306     C                   exsr      sr_Control
075400060306     C                   if        Cli_Abil = *on
075500060306     C                   eval      vgdKSU = woriIDC
075600050823     C                   write     tivgd000
075700060306     C                   endif
075800050823     C*
075900050823     C                   ENDSR
076000050823     C*------------------------------------------------------------------------*
076100010601
076200030811
076300030811
076400030811
076500030811      /TITLE Operazioni finali di chiusura *PGM di utilità
076600030811     C     exefin        BEGSR
076700030811     C*
076800030811     C* Chiusura TRUL13R
076900030811     C                   clear                   TRUL13DS
077000030811     C                   eval      I13TLA = 'C'
077100030811     C                   call      'TRUL13R'
077200030811     C                   parm                    TRUL13DS
077300030811     C*
077400030811     C* Chiusura TRUL33R
077500030811     C                   clear                   TRUL33DS
077600030811     C                   eval      I33TLA = 'C'
077700030908     C                   movel     TRUL33DS      KPJBU
077800030811     C                   call      'TRUL33R'
077900030908     C                   parm                    KPJBA
078000030811     C*
078100030811     C* Chiusura TISI95R
078200030811     C                   clear                   TISI95DS
078300030811     C                   eval      I95TLA = 'C'
078400030811     C                   call      'TISI95R'
078500030811     C                   parm                    TISI95DS
078600060113     C*
078700060113     C* Chiusura TISIT5R
078800060113     C                   clear                   TISIT0DS
078900060113     C                   eval      IT0TLA = 'C'
079000060113     C                   call      'TISIT5R'
079100060113     C                   parm                    TISIT0DS
079200030811     C*
079300030811     C                   ENDSR
079400030811     C***
079500060306
079600060306
079700060306     C*-------------------------------------------------------------------------*
079800060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
079900060306     C*-------------------------------------------------------------------------*
080000060306     C     sr_Control    begsr
080100060306     C*
080200060306     C* inizializzo a ok il flag d controllo abilitazione
080300060306     C                   eval      Cli_Abil = *on
080400060306     C
080500060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
080600060306     C                   clear                   woriIDC
080700060306     C                   eval      ktbCOD = 'VAP'
080800060306     C                   eval      ktbKE1 = oriIDC
080900060306     C     keytbe        chain     TNTBE01L
081000060306     C                   if        %found(Tntbe01l)
081100060306     C                   eval      woriIDC = TbeKe2
081200060306     C                   else
081300060306     C                   eval      Cli_Abil = *off
081400060306     C                   endif
081500060306     C*
081600060306     C                   endsr
081700010601
081800010601
081900990910
082000000613     C     *inzsr        BEGSR
082100990910     C*
082200990910     C     *entry        plist
082300040108     C                   parm                    InTipl
082400040108     C                   parm                    tivlrds
082500040108     C                   parm                    fivabds
082600030811     C*
082700030811     C* DEFINIZIONE CHIAVI
082800030811     C*
082900030811     C* TNTBE01L - parziale
083000030811     C     KEYtbe        klist
083100030811     C                   kfld                    KtbCOD                         *UTENTE
083200030811     C                   kfld                    KtbKE1                         *CODICE
083300010606     C*
083400010606     C* CALCOLA LA DATA CORRENTE
083500010606     C                   time                    wn14             14 0
083600010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
083700030811     C                   movel     wn14          oracor            6 0          *ORA  (6) IN HHMMSS
083800010606     C                   z-add     wn8           g08dat
083900010606     C                   z-add     *zeros        g08inv
084000010606     C                   movel     '0'           g08err
084100010606     C                   call      'XSRDA8'
084200010606     C                   parm                    wlbda8
084300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
084400000613     C*
084500000613     C                   ENDSR
