000100900914     H DECEDIT('0,') DATEDIT(*DMY.)
000200020422      * ------------------------------------------------------------------
000300020422      * TRASFERIMENTO CLIENTI DA UN P.O. AD UN ALTRO
000400020422      * ------------------------------------------------------------------
000500020418     FAzOrg01l  IF   E           K DISK
000600020422     FTabel00f  IF   E           K DISK
000700130801     fAZCMM01L  if   e           k disk
000800020422     FFlNuf01L  UF A E           K DISK
000900020422     FCnAco00f  IF A E           K DISK
001000020422     FCnClp00f  IF A E           K DISK
001100020422     FCnInd00f  IF A E           K Disk
001200020422     FFnCls01L  IF A E           K Disk
001300020422     FTfNtc01l  IF A E           K DISK
001400020422     FFnSpe01l  IF A E           K DISK
001500060118     FFnSp201l  IF A E           K DISK
001600020430     FFnAcr01l  IF A E           K DISK
001700020422     FTibs35d   CF   E             WORKSTN
001800020422     F                                     SFILE(Bs35S01:Nrr)
001900020417      *-----------------------------------------------------------
002000020422      * Schiere a tempo di compilazione
002100020422      *--------
002200051222     D Msg             S             78    DIM(20)CTDATA PERRCD(1)              MSG DI ERRORE
002300020417      *-----------------------------------------------------------
002400020422      * Schiere memorizzazione dati correnti
002500020422      *--------
002600020422     D $Ksc            S              7  0 DIM(14)
002700020422     D Ksm             S              7    DIM(20)
002800050915     d pog             S              3    DIM(250)
002801171129     d skNote          s              2    dim(999) inz
002900020422      *-----------------------------------------------------------
003000020418      * Ds di riferimento al file esterno AzUte00F
003100020422      *--------
003200020418     D AzuteDs       E DS                  ExtName(AzUte00F)
003300020422      *-----------------------------------------------------------
003400020418      * Ds per dati organigramma
003500020422      *--------
003600020418     D DDatiUte      E DS
003700020422      *-----------------------------------------------------------
003800020422      * Parametri per richiamo al pgm di controllo profilo utenti
003900020422      *--------
004000020422     D Tibs34Ds      E DS                  Inz
004100050915      *--------
004200050915     d dLat          e ds
004300090416     d dsfa          e ds
004400051222     d ds4w          e ds
004500050915     d dute01        e ds
004600050915     d Tibs02ds      e ds
004700050915     d Trul31ds      e ds                  INZ
004800130801     d TRMK43ds      e ds                  inz
004900020418      *--------
005000020422      * Tibs69r: lettura anagrafica clienti
005100020422      *--------
005200020422     D DsBs69        E DS                  ExtName(Tibs69Ds)
005300020422     D DsAco         E DS                  ExtName(CnAco00F)
005400020422     D DsInd         E DS                  ExtName(CnInd00F)
005500020422     D DsClp         E DS                  ExtName(CnClp00F)
005600020422     D DsCls         E DS                  ExtName(FnCls00F)
005700020422      *--------
005800020422     D Kpjba         E DS
005900020418      *--------
006000060915     d tibs73ds      e ds
006100090218     d ds4l          e ds
006101171129     d ds1T          e ds
006200100803
006300100803     d TrulIbanI0    e ds                  qualified
006400100803     d TrulIbanO0    e ds                  qualified
006500111026
006600111026     d tnta60ds      e ds
006700111026     d tnta61ds      e ds
006800100803
006900100803     d rqsformatname   s             10a
007000100803     d rqsData         s            256a
007100100803     d rqsdatasize     s             10i 0
007200100803     d rqsopcode       s             10a
007300100803     d rpyformatname   s             10a
007400100803     d rpyData         s            256a
007500100803     d rpydatasize     s             10i 0
007600100803     d rpyesito        s             10i 0
007601171129     d IndiceNota      s              3s 0
007700060915
007800020418     D                 DS                  Inz
007900020422     D V1SKsc                  1      7  0
008000020422     D V1SPou                  1      3  0
008100020422     D V1SCom                  4      7  0
008200020418      *--------
008300020418     D                 DS                  Inz
008400020422     D WNtcNk1                 1     11
008500020422     D WKci                    1      4  0
008600020422     D WSPou                   5      7  0
008700020422     D WSCom                   8     11  0
008800020418      *--------
008900020418     D                 DS                  Inz
009000020422     D WSpeCli                 1      7  0
009100020422     D WSPoU1                  1      3  0
009200020422     D WSCom1                  4      7  0
009300020430      *--------
009400020430     D                 DS                  Inz
009500020430     D WAcrCro                 1     10  0
009600020430     D WAcrKsc                 1      7  0
009700020430     D WAcrPoU                 8     10  0
009800020422      *---
009900020422      * Indici
010000020422      *---
010100020422     D X               S              2  0 Inz                                  *indice
010200020422      *--------
010300020422      * Definizioni
010400020422      *--------
010500020422     D WTrov           s              1
010600020423     D WOkF7           s              1
010700020422     D SavNrr          s                   like(Nrr)
010800020422     D WPoUCom         s                   like(V1CPoU)
010900020422     D NumKsc          s                   like(AcoKsc)
011000020422     D CodUt           s                   like(TblKut)
011100020422     D KKcc            s                   like(AcoKcc)
011200020422     D KKsc            s                   like(AcoKsc)
011300020422     D Cod             s                   like(TblCod)
011400020422     D Key             s                   like(TblKey)
011500020422     D NtcAplBis       s                   like(NtcApl)
011600020422     D NtcNk1Bis       s                   like(NtcNk1)
011700020422     D NtcNk2Bis       s                   like(NtcNk2)
011800020422     D SpeCliBis       s                   like(SpeCli)
011900020422     D SpeFlsBis       s                   like(SpeFls)
012000020430     D AcrCroBis       s                   like(AcrCro)
012100050915     d wabi            s                   like(UteAut)
012200050915     d w003a           s              3
012300040409     d w7_acrcro       s                   like(v1sksc)
012400060118     d ksp2cli         s                   like(sp2cli)
012500060118     d ksp2cod         s                   like(sp2cod)
012600090218     d $nospe          s               n
012700050915
012800040719     D WLBDAT          DS                  INZ
012900040719     D  G02DAT                 1      8  0
013000040719     D  G02INV                 9     16  0
013100040719     D  G02ERR                17     17
013200040719     D  G02TGI                18     22  0
013300060915
013400060915     d pgmstatus      sds
013500060915     d job_name              244    253
013600060915     d user                  254    263
013700100803
013800100803      // - Coordinate bancarie
013900100803     d TrulIbanR       pr                  extpgm('TRULIBANR')
014000100803     d  rqsOpCode                    10a   const
014100100803     d  rpyEsito                     10i 0
014200100803     d  rqsFormatName                10a   const
014300100803     d  rqsData                     256a   options(*varsize)
014400100803     d  rqsDataSize                  10i 0 const
014500100803     d  rpyFormatName                10a   const
014600100803     d  rpyData                     256a   options(*varsize)
014700100803     d  rpyDataSize                  10i 0 const
014800020422      * ------------------------------------------------------------------
014900981228     C     *ENTRY        PLIST
015000020417     C                   PARM                    Kpjba
015100020417      *
015200020417      * Reperisco dati job
015300020418     C                   EXSR      DatiJob
015400020418      *
015500020418      * Caricamento tabelle
015600020418     C                   EXSR      CarTab
015700020417      *                                                                ---
015800990107     C     INIZIO        TAG
015900020423      *
016000020423      * Inizializzazione Control
016100020423     C                   EXSR      InzCtl
016200020417      *
016300020422      * Scrivo il control
016400020417     C                   seton                                        35
016500020417     C                   WRITE     Bs35C01
016600020417     C                   setoff                                       35
016700020418      *
016800020418      * Scrivo la pagina del Sfl con righe vuote
016900020422     C                   DO        14            Nrr
017000020417     C                   WRITE     Bs35S01
017100990107     C                   ENDDO
017200020417     C                   z-add     1             Nrr
017300020417      *
017400981228     C     FOR01         TAG
017500020417     C                   WRITE     Bs35Z01
017600020417     C                   EXFMT     Bs35C01
017700020422      *
017800020417      * F03 - Fine
017900020520     C   KC
018000020520     COR 91              GOTO      FINE
018100020417      *
018200020517     C                   setoff                                       90
018300020417     C                   setoff                                       424344
018400020417     C                   setoff                                       454647
018500111026     c                   setoff                                       41
018600020422      *
018700020417      * Controlli
018800020423     C                   EXSR      CtrD01
018900020418     C   90              GOTO      FOR01
019000020417      *
019100111026     c                   IF        not *inkh and not *inkf
019200111026     c                   goto      for01
019300111026     c                   ENDIF
019400020422      *
019500020422      * F6 - Conferma: elaboro duplica clienti
019600111027      * F8 - Conferma + stampa abilitazioni
019700111027     c                   IF        *inkf or *inkh
019800111027     c                   EXSR      Elab
019900111026     c                   ENDIF
020000111027
020100111026     c                   IF        (*inkf or *inkh) and *in90
020200111026     c                   goto      for01
020300111026     c                   ENDIF
020400020417      *
020500020418     C                   GOTO      INIZIO
020600020417      *
020700981228     C     FINE          TAG
020800900911     C                   SETON                                        LR
020900020423      * ------------------------------------------------------------------
021000020423      * INIZIALIZZAZIONE CONTROL
021100020423      * ------------------------------------------------------------------
021200020423     C     INZCTL        BEGSR
021300020423      *
021400020423     C                   z-add     1             CodUt
021500020423     C                   z-add     DutKci        KKcc
021600020423      *
021700020423     C                   clear                   V1CPoU
021800020423     C                   clear                   V1DPoU
021900020423     C                   clear                   V1SPoU
022000020423     C                   clear                   V1SCom
022100020423     C                   clear                   V1SDcI
022200020423     C                   clear                   V1SCmV
022300020423     C                   clear                   V1SDmV
022400020423     C                   clear                   V1SCmN
022500020423     C                   clear                   V1SIfV
022600020423     C                   clear                   V1SIfN
022700020423     C                   clear                   V2SDmN
022800020423     C                   clear                   V2SDfV
022900020423     C                   clear                   V2SDfN
023000111026     c                   clear                   v1sopz
023100111026     c                   clear                   v1sabi
023200020423      *
023300020423      * Decodifica del P.O.
023400020423     C                   z-add     DutPou        V1CPou
023500020423     C                   eval      V1DPoU = *blanks
023600020423     C     V1CPoU        Chain     AzOrg01l                           30
023700020423   1aC                   IF        *in30 = *off
023800020423     C                   eval      V1DPoU = OrgDes
023900020423   1-C                   ENDIF
024000020423      *
024100020423     C                   ENDSR
024200020417      * ------------------------------------------------------------------
024300020417      * CONTROLLI
024400020417      * ------------------------------------------------------------------
024500981228     C     CTRD01        BEGSR
024600020517     C   91              GOTO      ENDCTR
024700020423      *
024800020423      * Indicatore per segnalare la richiesta F7
024900020423     C                   eval      WOkF7 = *off
025000020418      *
025100020417      * P.O
025200020418      * Obbligatorietà e correttezza P.O.
025300020417      *
025400020422     C                   eval      V1DPoU = *blanks
025500020419   1aC                   IF        V1CPoU = 0
025600020417     C                   eval      V1CMsg = MSG(1)
025700020426     C                   seton                                        4290
025800990107     C                   GOTO      ENDCTR
025900020419   1-C                   ENDIF
026000020417      *
026100020419     C     V1CPoU        Chain     AzOrg01l                           30
026200020419   1aC                   IF        *in30 = *on or OrgFva <>' '
026300020417     C                             or (OrgFag <> 'A' and OrgFag <> 'F')
026400020417     C                   eval      V1CMsg = MSG(2)
026500020426     C                   seton                                        4290
026600990107     C                   GOTO      ENDCTR
026700020419   1-C                   ENDIF
026800020417      *
026900020419     C                   eval      V1DPoU = OrgDes
027000020417      *
027100050915      * IL PO per cui creare l'anagrafica deve essere gestito dall'utente
027200050915     c                   Move      v1cpou        w003a
027300050915     c     w003a         Lookup    pog                                    30
027400020419   1aC                   IF        *in30 = *off
027500111026     C                   eval      V1CMsg = MSG(2)
027600020426     C                   seton                                        4290
027700990107     C                   GOTO      ENDCTR
027800020419   1-C                   ENDIF
027900020419      *
028000020419      * LETTURA SFL
028100020417     C                   z-add     1             Nrr
028200020422     C                   clear                   WTrov
028300020417      *
028400020426   1aC     Nrr           DOWLE     14
028500020417     C     Nrr           CHAIN     BS35S01                            31
028600020419   2aC                   IF        *in31 = *OFF
028700020419      *
028800020419      * Pulisco campi output/decodifiche
028900020422     C                   clear                   V1SCmV
029000020422     C                   clear                   V1SDmV
029100020422     C                   clear                   V1SIfV
029200020422     C                   clear                   V2SDmN
029300020422     C                   clear                   V2SDfV
029400020422     C                   clear                   V2SDfN
029500020508     C                   clear                   V2Stop
029600111026
029700111027      * Controllo che se immessa l'opzione '5' o '8' sia inserito il codice
029800111026      * cliente da visualizzare
029900111026     c                   IF        V1Sopz <> *blanks and V1Sksc = 0
030000111026     c                   eval      V1Cmsg = msg(3)
030100111026     c                   seton                                        4190
030200111026     c                   goto      endct1
030300111026     c                   ENDIF
030400111027
030500111027      * Controllo che se immessa l'opzione '8' il cliente abbia delle
030600111027      * abilitazioni
030700111027     c                   IF        V1Sopz = '8' and V1Sabi = *blanks
030800111027     c                   eval      V1Cmsg = msg(3)
030900111027     c                   seton                                        4190
031000111027     c                   goto      endct1
031100111027     c                   ENDIF
031200020419      *
031300020419      * Ricerca codice commerciale
031400020422     C     '?'           scan      V1SCmN                                 21
031500020419   3aC                   IF        *in21 = *on
031600130801     C                   clear                   TRMK43ds
031700130801     C                   move      V1CPoU        iMK43fil
031800130801     C                   call      'TRMK43R'
031900020419     C                   PARM                    KPJBA
032000130801     C                   PARM                    TRMK43ds
032100020419      *
032200130801     c                   if        oMK43err = *off  and
032300130801     c                             oMK43fxx = *blank
032400130801     C                   movel     oMK43cmm      V1SCmN
032500130801     C                   movel     oMK43des      V2SDmN
032600130801     c                   endif
032700020419   3-C                   ENDIF
032800020419      *
032900020419      * Controllo che il P.O cliente esista e non sia annullato
033000090416      * si può duplicare all'interno delle stesso p.o.
033100020419   3aC                   IF        V1SPoU > 0
033200020422     C     V1SPoU        Chain     AzOrg01l                           21
033300020422   4aC                   IF        *in21 = *on or OrgFva <>' '
033400020419     C                             or (OrgFag <> 'A' and OrgFag <> 'F')
033500020419     C                   eval      V1CMsg = MSG(2)
033600020426     C                   seton                                        4390
033700020419     C                   GOTO      ENDCT1
033800020419   4-C                   ENDIF
033900020419   3-C                   ENDIF
034000020417      *
034100020417      * Ricerca alfabetica
034200020419   3aC                   IF        *inkg  =*on and V1SDci <> *blanks
034300020417     C                             and V1SCom = *zeros
034400020419     C                   movel     RsUt          PAXDUT           30
034500020419      * Paxsta=0 non escludo annullati
034600020419     C                   z-add     0             PAXSTA
034700020422     C                   z-add     DutKci        PAXCCC
034800020417     C                   movel     V1SDCi        PAXDMT           48
034900020417     C                   clear                   ACOKSC
035000020419   4aC                   IF        V1SPou >0
035100020417     C                   movel     V1SPou        PAXFLR
035200020419   4-C                   ENDIF
035300020417     C                   clear                   PAXKSM
035400020417     C                   clear                   PAXKCM
035500020417     C                   clear                   PAXKDM
035600020426     C                   z-add     14            PAXNUM
035700020417      *
035800990107     C                   CALL      'XALFA3BR'
035900990107     C                   PARM                    PAXDUT
036000020422     C                   PARM                    CodUt             1 0
036100990107     C                   PARM                    PAXDMT
036200990107     C                   PARM                    PAXCCC            4 0
036300990107     C                   PARM                    PAXSTA            1 0
036400990107     C                   PARM                    PAXFLR           90
036500990107     C                   PARM                    PAXDIT            3
036600991203     C                   PARM                    PAXNUM            2 0
036700990107     C                   PARM                    PAXKCM           80
036800990107     C                   PARM                    PAXKSM          140
036900990107     C                   PARM                    PAXKDM           60
037000020417      *
037100020419      * Non trovato
037200020419   4aC                   IF        PaxSta > -1
037300020417      * Imposto a video i codici selezionati
037400020417     C                   movea     PAXKSM        KSM
037500020422     C                   z-add     Nrr           SavNrr
037600020417      *
037700020417     C                   z-add     1             BB                2 0
037800020419   5aC     KSM(BB)       DOWNE     *blanks
037900020426     C     Nrr           ANDLE     14
038000020419   6aC                   IF        BB > 1
038100020412     C                   UPDATE    BS35S01
038200020417     C                   add       1             Nrr
038300020422     C     Nrr           chain     Bs35S01                            21
038400020419   6-C                   ENDIF
038500020417      *
038600020419   6aC                   IF        V1SCom = 0
038700020417     C                   move      Ksm(BB)       V1SKsc
038800020417     C                   add       1             BB
038900020419   6-C                   ENDIF
039000020419   5-C                   ENDDO
039100020417      *
039200020419   5aC                   IF        Ksm(2) <> *BLANKS
039300020417     C                   UpDate    Bs35S01
039400020417      *
039500020417     C                   z-add     SavNrr        NRR
039600020422     C     NRR           chain     Bs35S01                            21
039700020419   5-C                   ENDIF
039800020419   4-C                   ENDIF
039900020423      * segnalo che ho richiesto la ricerca alfabetica
040000020423     C                   eval      WOkF7 = *on
040100020419   3-C                   ENDIF
040200020419      *
040300020423      * CODICE CLIENTE
040400020423     C                   eval      V1SDci = *blanks
040500020423      *
040600020423      * Se ho trovato qualcosa cerco decodifiche
040700020423   3aC                   IF        V1SKsc > 0
040800020417      * Lettura anagrafica cliente
040900020417     C                   clear                   DsBs69
041000020418     C                   move      0             I69Kcc
041100020418     C                   z-add     V1SKsc        I69Kac
041200051222     C                   z-add     V1SKsc        I69Kin
041300020418     C                   z-add     V1SKsc        I69Kcp
041400020417     C                   movel     Knsif         I69Sif
041500981228     C                   CALL      'TIBS69R'
041600981228     C                   PARM                    DSBS69
041700981228     C                   PARM                    DSACO
041800981228     C                   PARM                    DSIND
041900981228     C                   PARM                    DSCLP
042000981228     C                   PARM                    DSCLS
042100020417      * errore
042200020423   4aC                   if        O69Err <> ' '
042300020426     C  n90              eval      V1CMsg = Msg(4)
042400020426     C  n90              seton                                        4490
042500020423      * Pulisco i campi I/O dels sfl
042600020423     C                   clear                   V1SCmN
042700020423     C                   clear                   V1SIfN
042800981228     C                   GOTO      ENDCT1
042900020423   4-C                   endif
043000020424      * Clente scaduto
043100020424   4aC                   if        O69Err =  ' ' and acoflg <>' '
043200020426     C  n90              eval      V1CMsg = Msg(11)
043300020426     C  n90              seton                                        4490
043400020424      * Pulisco i campi I/O dels sfl
043500020424     C                   clear                   V1SCmN
043600020424     C                   clear                   V1SIfN
043700020424     C                   GOTO      ENDCT1
043800020424   4-C                   endif
043900090416      * devo controllare se il cliente ha un codice pagamento fattura che
044000090416      * è gestibile solo da sede
044100090416      * in questo caso errore perchè non posso duplicare
044200090416     c                   clear                   dsfa
044300090416     c                   eval      cod = 'FA'
044400090416     c                   eval      key = %subst(indcdp:4:3) + '1'
044500090416     c     ktab          chain     tabel00f
044600090416     c                   if        %found(tabel00f) and
044700090416     c                             tblflg = *Blanks
044800090416     c                   eval      dsfa = tbluni
044900090416     c                   endif
045000090416     c                   if        §fauti = 'S'
045100090416     c                   eval      v1cmsg = msg(16)
045200090416     c                   eval      *in44 = *on
045300090416     c                   eval      *in90 = *on
045400090416     c                   Goto      endct1
045500090416     c                   endif
045600110715
045700110715      * NON è possibile duplicare il codice quando ha
045800110715      * la fattura settimanale.
045900110715     c                   IF        CLPfft = 1
046000110715     c                   eval      v1cmsg = msg(17)
046100110715     c                   eval      *in44 = *on
046200110715     c                   eval      *in90 = *on
046300110715     c                   Goto      endct1
046400110715     c                   endif
046500020418      *
046600020422      * RAGIONE SOCIALE
046700020418     C                   eval      V1SDci = AcoRag
046800020426      *
046900020426      * CODICE COMMERCIALE
047000020426   4aC                   IF        V1SCom = 0 and V1sPoU <> 0
047100020426     C                             and *in90=*off
047200020426     C  n90              eval      V1CMsg = Msg(4)
047300020426     C  n90              seton                                        4490
047400020426      * Pulisco i campi I/O dels sfl
047500020426     C                   clear                   V1SCmN
047600020426     C                   clear                   V1SIfN
047700020426     C                   GOTO      ENDCT1
047800020426   4-C                   ENDIF
047900051223      * Se cambio cliente sul subfile
048000051223     c                   If        v1sksc <> v1scli
048100051223     c                   Clear                   v1sabl
048200051223     c                   Clear                   v1scon
048300051223     c                   Clear                   v1siva
048400051223     c                   Eval      v1scli = v1sksc
048500051223     c                   EndIf
048600051222      * Se cliente bloccato errore forzabile e riporto il blocco sul nuovo cliente
048700051222     c                   If        acoabl <> *Blanks and v1sabl = *Blanks
048800051222     c                   Eval      v1cmsg = msg(13)
048900051222     c                   Eval      %subst(v1cmsg:36:1) = acoabl
049000130321      * se blocco "7" (blocco automatico) visualizzo blocco "8"
049100130321     c                   IF        ACOabl = '7'
049200130321     c                   eval      %subst(V1Cmsg:36:1) = '8'
049300130321     c                   ENDIF
049400051222     c                   Eval      *In44 = *On
049500051222     c                   Eval      *In90 = *On
049600051222     c                   Eval      v1sabl = '1'
049700051222     c                   Goto      endct1
049800051222     c                   EndIf
049900051222      * Se cliente con stato del credito errore forzabile e riporto il blocco sul nuovo cliente
050000051222     c                   If        clpcon <> *Blanks and v1scon = *Blanks
050100051222     c                   Clear                   ds4w
050200051222     c                   Eval      cod = '4W'
050300051222     c                   Move(p)   clpcon        w003a
050400051222     c                   Eval      key = w003a
050500051222     c     kTab          Chain     Tabel00f
050600051222     c                   If        %Found(Tabel00f) and
050700051222     c                             tblflg = *Blanks
050800051222     c                   Eval      ds4w = tbluni
050900051222     c                   EndIf
051000051222     c                   Eval      v1cmsg = msg(14)
051100051222     c                   Eval      %subst(v1cmsg:15:25) = §4wdes
051200051222     c                   Eval      *In44 = *On
051300051222     c                   Eval      *In90 = *On
051400051222     c                   Eval      v1scon = '1'
051500051222     c                   Goto      endct1
051600051222     c                   EndIf
051700051222      * Se cliente con es.IVA errore forzabile e NON riporto il blocco sul nuovo cliente
051800051222     c                   If        (inddpr <> *Zeros or indnpn <> *Zeros or
051900051222     c                              indnpr <> *Zeros) and v1siva = *Blanks
052000051222     c                   Eval      v1cmsg = msg(15)
052100051222     c                   Eval      *In44 = *On
052200051222     c                   Eval      *In90 = *On
052300051222     c                   Eval      v1siva = '1'
052400051222     c                   Goto      endct1
052500051222     c                   EndIf
052600130801      *
052700111026      * Controllo se ci sono abilitazioni
052800111026     c                   clear                   tnta61ds
052900111026     c                   eval      TA61tla = 'C'
053000111026     c                   eval      TA61ksc = V1Sksc
053100111026     c                   call      'TNTA61R'
053200111026     c                   parm                    kpjba
053300111026     c                   parm                    tnta61ds
053400111026     c                   IF        TA61flg = '1'
053500111026     c                   eval      V1Sabi = '*'
053600111026     c                   ELSE
053700111026     c                   clear                   V1Sabi
053800111026     c                   ENDIF
053900130801      *
054000111027      * Opzione di visualizzazione anagrafica
054100111026     c                   IF        V1Sopz = '5' and V1Sksc <> 0
054200111026     c                   clear                   tnta60ds
054300111026     c                   eval      TA60flg = 'I'
054400111026     c                   eval      TA60nrv = V1Sksc
054500111026     c                   eval      TA60int = 'S'
054600111026     c                   call      'TNTA60R'
054700111026     c                   parm                    kpjba
054800111026     c                   parm                    tnta60ds
054900111026     c                   clear                   V1Sopz
055000111027      * imposto il flag di trovato per non dare errore quando riemetto il subfile
055100111027     c                   eval      wtrov = '1'
055200111026     c                   goto      endct1
055300111026     c                   ENDIF
055400130801      *
055500111027      * Opzione di visualizzazione abilitazioni
055600111027     c                   IF        V1Sopz = '8' and V1Sksc <> 0
055700111027     c                   clear                   tnta61ds
055800111027     c                   eval      TA61tla = 'V'
055900111027     c                   eval      TA61ksc = V1Sksc
056000111027     c                   call      'TNTA61R'
056100111027     c                   parm                    kpjba
056200111027     c                   parm                    tnta61ds
056300111027     c                   clear                   V1Sopz
056400111027      * imposto il flag di trovato per non dare errore quando riemetto il subfile
056500111027     c                   eval      wtrov = '1'
056600111027     c                   goto      endct1
056700111027     c                   ENDIF
056800020508      *
056900020508      * NR.STOP IN DISTINTA
057000020508     C     V1SKsc        chain     FnCls01l                           21
057100020508     C  N21              move      Clsstp        V2Stop
057200020422      *
057300020422      * COD.COMMERCIALE E DECODIFICA
057400020422     C                   eval      V1SDmV = *blanks
057500020422     C                   eval      V1SCmV = ClpAge
057600020423      *
057700130801     C     V1ScmV        chain     AZCMM000
057800130801     c                   if        %found(AZCMM01L)
057900130801     C                   eval      V1SDmV = CMMdes
058000130801     c                   endif
058100020418      *
058200020422      * S.CONTO INTESTAZIONE FATTURA E DECODIFICA
058300020422     C                   eval      V2SDfV = *blanks
058400020418     C                   eval      V1SIfv = ClpScf
058500020418      *
058600020418     C                   eval      KKsc = ClpScf
058700020419     C     KAco          chain     CnAco00f                           21
058800020422     C  N21              eval      V2SDfV = AcoRag
058900020418      *
059000020426      * CONTROLLO I CAMPI I/O DEL SFL (accendo 90)
059100020422      *
059200020422      * CODICE COMMERCIALE NUOVO
059300020426   4aC                   IF        V1SCmN<>*blanks and V1SCmN <> *zeros
059400020419      *
059500020419      * Controllo validità del codice commerciale
059600020422     C                   testn                   V1SCmN               36
059700020430   5aC                   IF        *in36 = *off
059800020426     C  n90              eval      V1CMsg = Msg(7)
059900020426     C  n90              seton                                        9046
060000020419     C                   GOTO      ENDCT1
060100020426   5-C                   ENDIF
060200020419      *
060300130801      * Controllo esistenza codice commerciale in anagrafica
060400020419      *
060500130801     c                   move      V1ScmN        CMMcod
060600130801     c     CMMcod        chain     AZCMM000
060700020422      * Codice commerciale errato
060800020422      * Il P.O del commerciale deve essere uguale al nuovo P.O
060900020422     C                   Movel     V1SCmN        WPoUCom
061000130801   5aC                   IF        Not %found(AZCMM01L)  or
061100020423     C                             WPoUCom <> V1CPoU
061200020426     C  n90              eval      V1CMsg = Msg(7)
061300020426     C  n90              seton                                        4690
061400020419     C                   GOTO      ENDCT1
061500020426   5-C                   ENDIF
061600020419      *
061700020419      * Decodifica codice commerciale
061800020422     C                   eval      V2SDmN = TBLUNI
061900020426   4eC                   ELSE
062000020422      * Cod. Commerciale obbligatorio
062100020430      * lo segnalo solo se non provengo dalla ricerca
062200020430     C                   if        WOkF7 =*off
062300020426     C  n90              eval      V1CMsg = Msg(8)
062400020426     C  n90              seton                                        4690
062500020422     C                   GOTO      ENDCT1
062600020430     C                   endif
062700020426   4-C                   ENDIF
062800020422      *
062900020422      * S.CONTO INTESTAZIONE FATTURA NUOVO
063000020422     C                   clear                   V2SDfN
063100020422   4aC                   IF        V1SIfN <> *zeros
063200020422     C                   move      V1SIfN        WIfN              4 0
063300020422   5aC                   IF        WIfN=0 or WIfn=8888 or WIfN=9999
063400020426     C  n90              eval      V1CMsg = Msg(9)
063500020426     C  n90              seton                                        4790
063600020422     C                   GOTO      ENDCT1
063700020422   5eC                   ELSE
063800020422      *
063900020422      * Controllo esistenza su cnaco00f
064000020422     C                   move      V1SIfN        KKsc
064100020422     C     KAco          chain     CnAco00f                           21
064200020424   6aC                   if        *in21 =*off and acoflg = ' '
064300020424     C                   eval      V2SDfN = AcoRag
064400020424   6eC                   else
064500020426     C  n90              eval      V1CMsg = Msg(9)
064600020426     C  n90              seton                                        4790
064700020422     C                   GOTO      ENDCT1
064800020424   6-C                   endif
064900020422   5-C                   ENDIF
065000170306      *?Se arrivo qua il nuovo sottoconto fattura è stato impostato ed esiste
065100170306      *?ora però devo controllare che non sia un codice in contenzioso
065200170306      *?nel caso do errore bloccante
065300170306     c     kACO          chain     CNCLP00F
065400170306   5ac                   IF        %found(CNCLP00F) and
065500170306     c                             CLPcon <> *blanks
065600170306     c                   clear                   ds4W
065700170306     c                   eval      cod = '4W'
065800170306     c                   move(p)   CLPcon        w003a
065900170306     c                   eval      key = w003a
066000170306     c     kTAB          chain     TABEL00F
066100170306   6ac                   IF        %found(TABEL00F) and
066200170306     c                             TBLflg = *blanks
066300170306     c                   eval      ds4W = TBLuni
066400170306   6-c                   ENDIF
066500170306   6ac                   IF        §4Wtip <> *blanks
066600170306     c                   eval      V1Cmsg = msg(06)
066700170306     c                   eval      %subst(V1cmsg:53:25) = §4Wdes
066800170306     c                   eval      *in47 = *On
066900170306     c                   eval      *in90 = *On
067000170306     c                   goto      endct1
067100170306   6-c                   ENDIF
067200170306   5-c                   ENDIF
067300020422   4-C                   ENDIF
067400020417      *
067500020417      * segnalo che ho caricato qualcosa senza errore
067600020417     C                   eval      WTrov = '1'
067700020422   3-C                   ENDIF
067800990107     C     ENDCT1        TAG
067900020417      *
068000020417     C                   UPDATE    Bs35S01
068100020419   2-C                   ENDIF
068200020417      * Se trovo errore esco
068300020423      * Non esco se provengo da ricerca alfabetica per
068400020423      * permettere la decodifica del codice cliente
068500020423   2aC                   IF        *in90= *on and WOkF7 = *off
068600020426     C                   z-add     15            NRR               4 0
068700020422     C                   GOTO      ENDCTR
068800990107     C                   ELSE
068900020417     C                   add       1             NRR
069000020419   2-C                   ENDIF
069100020419   1-C                   ENDDO
069200020422      *
069300020422      * CONTROLLO CHE NEL SFL NON ESISTANO CODICI CLIENTI DOPPI
069400020422     C                   z-add     1             Nrr
069500020422     C                   z-add     1             X
069600020423     C                   reset                   $Ksc
069700020422   1aC     Nrr           DOWLE     14
069800020422     C     Nrr           chain     Bs35S01                            31
069900020422     C     V1SKsc        lookup    $Ksc                                   30
070000020423   2aC                   if        V1SKsc <>0
070100020423   3aC                   if        *in30 = *on
070200020422      * Nel SFL esiste già lo stesso codice, segnalo l'errore
070300020426     C                   seton                                        90
070400020422     C                   eval      V1CMsg = Msg(10)
070500020422     C                   GOTO      ENDCTR
070600020423   3eC                   else
070700020422     C                   move      V1SKsc        $Ksc(x)
070800020422     C                   add       1             X
070900020423   3-C                   endif
071000020423   2-C                   endif
071100020422     C                   add       1             NRR
071200020423   1-C                   ENDDO
071300020417      *
071400020417      * Se non impostato nemmeno un codice segnalo errore
071500020419   1aC                   IF        *in90 =*off and WTrov = ' '
071600020417     C                   eval      V1CMsg = Msg(5)
071700020426     C                   seton                                        90
071800020419   1-C                   ENDIF
071900020417      *
072000990107     C     ENDCTR        ENDSR
072100020417      *----------------------------------------------------------------
072200020417      * CREO L'ANAGRAFICA CLIENTI
072300020417      *----------------------------------------------------------------
072400981228     C     ELAB          BEGSR
072500130801      *
072600020417     C                   z-add     1             Nrr
072700020426   1aC     Nrr           DOWLE     14
072800020417     C     Nrr           chain     Bs35S01                            31
072900020422   2aC                   IF        *in31=*off and V1SCom <>0
073000020422      * Prelevo il nuovo numero codice cliente
073100020422     C                   EXSR      Caric
073200020422      *
073300020422      * SCRITTURA CNACO
073400020422     C                   z-add     DutKci        KKcc
073500020422     C                   eval      KKsc = V1SKsc
073600130801      *
073700060915     c                   clear                   dsaco
073800060915     c                   clear                   dsind
073900060915     c                   clear                   dsclp
074000060915     c                   clear                   dscls
074100020422      *
074200020422     C     KAco          chain     CnAco00f
074300020422   3aC                   if        %found(CnAco00f) and AcoFlg =' '
074400020430      * cod.cliente
074500020422     C                   move      NumKsc        AcoKsc
074600020430      * cod. filiale per trasmissione
074700020422     C                   move      V1CPoU        AcoFlt
074800020430     C                   move      *blanks       acoftr
074900020430     C                   move      *zeros        acodtr
075000071128     C                   move      dateu         acoduv
075100020422     C                   write     CnAco000
075200020422      *
075300020422      * SCRITTURA CNIND
075400020422     C     KAco          chain     CnInd00f
075500060112   3aC                   if        %found(CnInd00f)
075600020430      * cod. cliente
075700020422     C                   move      NumKsc        IndKsc
075800051222      * Non riporto i dati del protocollo x esenzione IVA
075900051222     c                   Clear                   indnpr
076000051222     c                   Clear                   inddpr
076100051222     c                   Clear                   indnpn
076200020422     C                   write     CnInd000
076300020422   3-C                   endif
076400020422      *
076500020422      * SCRITTURA CNCLP
076600020422     C     KAco          chain     CnClp00f
076700060112   3aC                   if        %found(CnClp00f)
076800020430      * cod. cliente e cod. agente
076900020422     C                   move      NumKsc        ClpKsc
077000020422     C                   move      V1SCmN        ClpAge
077100020430      * intestazione fatture
077200020422   4aC                   if        V1SIfN > 0
077300020422     C                   move      V1SIfN        ClpScf
077400020430     C                   else
077500020430     C                   move      ClpKsc        ClpScf
077600020422   4-C                   endif
077700020430      *
077800020422     C                   write     CnClp000
077900020422   3-C                   endif
078000020422      *
078100020422      * SCRITTURA FNCLS
078200020422     C     V1SKsc        chain     FnCls01l
078300060112   3aC                   if        %found(FnCls01l)
078400020422     C                   move      NumKsc        ClsKsc
078500020430      * n.ro stop in distinta
078600020430     C                   move      *zeros        clsstp
078700020422     C                   write     FnCls000
078800020422   3-C                   endif
078900060915      * Memorizzo l'immagine seguente per storicizzare le variazioni
079000060915     c                   exsr      scriviACV
079100100803
079200100803      /free
079300100803       //?Pagamento DANNI --> DN
079400100803       //?Recupero le coordinate bancarie del vecchio codice
079500100803         rqsOpCode = 'SEARCH';
079600100803         clear rpyEsito;
079700100803         clear TrulIbanI0;
079800100803         clear TrulIbanO0;
079900100803         TrulIbanI0.KSC = v1sksc;
080000100803         TrulIbanI0.PAG = 'DN';
080100130304         TrulIbanI0.PGM   = 'TIBS35R' ;
080200100803         rqsFormatName = 'TRULIBANI0';
080300100803         rpyFormatName = 'TRULIBANO0';
080400100803         rqsDataSize   = %size(TrulIbanI0);
080500100803         rpyDataSize   = %size(TrulIbanO0);
080600100803         TrulIbanR (rqsOpCode:rpyEsito:
080700100803                    rqsFormatName:TrulIbanI0:rqsDataSize:
080800100803                    rpyFormatname:TrulIbanO0:rpyDataSize);
080900100803       //?Trovo le coordinate le vado a scrivere sul nuovo codice
081000100803         IF  rpyEsito = 0;
081100100803           rqsOpCode = 'INSERT';
081200100806           TrulIbanI0.KSC  = numksc;
081300100806           TrulIbanI0.CCB  = TrulIbanO0.CCB;
081400100806           TrulIbanI0.ABI  = TrulIbanO0.ABI;
081500100806           TrulIbanI0.CAB  = TrulIbanO0.CAB;
081600100806           TrulIbanI0.IBAN = TrulIbanO0.IBAN;
081700100806           TrulIbanI0.BIC  = TrulIbanO0.BIC;
081800100803           TrulIbanR (rqsOpCode:rpyEsito:
081900100803                      rqsFormatName:TrulIbanI0:rqsDataSize:
082000100803                      rpyFormatname:TrulIbanO0:rpyDataSize);
082100100803         ENDIF;
082200100803       //?Pagamento NOTE ACCREDITO --> NA
082300100803       //?Recupero le coordinate bancarie del vecchio codice
082400100803         rqsOpCode = 'SEARCH';
082500100803         clear rpyEsito;
082600100803         clear TrulIbanI0;
082700100803         clear TrulIbanO0;
082800100803         TrulIbanI0.KSC = v1sksc;
082900100803         TrulIbanI0.PAG = 'NA';
083000130304         TrulIbanI0.PGM   = 'TIBS35R' ;
083100100803         rqsFormatName = 'TRULIBANI0';
083200100803         rpyFormatName = 'TRULIBANO0';
083300100803         rqsDataSize   = %size(TrulIbanI0);
083400100803         rpyDataSize   = %size(TrulIbanO0);
083500100803         TrulIbanR (rqsOpCode:rpyEsito:
083600100803                    rqsFormatName:TrulIbanI0:rqsDataSize:
083700100803                    rpyFormatname:TrulIbanO0:rpyDataSize);
083800100803       //?Trovo le coordinate le vado a scrivere sul nuovo codice
083900100803         IF  rpyEsito = 0;
084000100803           rqsOpCode = 'INSERT';
084100100806           TrulIbanI0.KSC  = numksc;
084200100806           TrulIbanI0.CCB  = TrulIbanO0.CCB;
084300100806           TrulIbanI0.ABI  = TrulIbanO0.ABI;
084400100806           TrulIbanI0.CAB  = TrulIbanO0.CAB;
084500100806           TrulIbanI0.IBAN = TrulIbanO0.IBAN;
084600100806           TrulIbanI0.BIC  = TrulIbanO0.BIC;
084700100803           TrulIbanR (rqsOpCode:rpyEsito:
084800100803                      rqsFormatName:TrulIbanI0:rqsDataSize:
084900100803                      rpyFormatname:TrulIbanO0:rpyDataSize);
085000100803         ENDIF;
085100100803      /end-free
085200100803
085300060112   3-C                   endif
085400020422      *
085500020422      * SCRITTURA TFNTC
085600020422      * Valorizzo la chiave
085700020426     C                   eval      NtcAplBis ='C'
085800020422     C                   eval      WNtcNk1 = *blanks
085900020422     C                   z-add     DutKci        WKci
086000020422     C                   eval      WSPou  = V1SPou
086100020422     C                   eval      WSCom  = V1SCom
086200020422     C                   eval      NtcNk1Bis = WNtcNk1
086300020422     C                   eval      NtcNk2Bis = *blanks
086400020422      *
086500020423      * Leggo tutti i tipi nota del cliente che abbiano la chiave 2
086600020423      * a blanks
086700020422     C     KntcBis       setll     TfNtc01l
086800020423     C                   read      TfNtc01l                               32
086900020422   3aC     *in32         DOWEQ     *off
087000020426     C     NtcNk1        ANDEQ     NtcNk1Bis
087100020426     C     NtcNk2        ANDEQ     *blanks
087200020426   4aC                   IF        NtcFlt <> 'A'
087201171129       //solo note che posso duplicare
087202171129        IF  %lookup(NTCtnt:skNote) = 0;
087300020430      * cod. cliente
087400020422     C                   eval      WNtcNk1 = NtcNk1
087500020422     C                   eval      WSPou  = V1CPoU
087600020426     C                   move      NumKsc        WSCom
087700020422     C                   eval      NtcNk1 = WNtcNk1
087800040806     C                   move      dateu         ntcntr
087900020430     C                   move      *blanks       ntcflt
088000020422     C                   write     TfNtc
088001171129        ENDIF;
088100020422   4-C                   ENDIF
088200020423     C                   read      TfNtc01l                               32
088300020422   3-C                   ENDDO
088400020422      *
088500020422      * Scrittura FnSpe
088600020422      * Valorizzo la chiave
088700020422     C                   eval      SpeFlsBis ='L'
088800020422     C                   eval      WSPou1 = V1SPou
088900020422     C                   eval      WSCom1 = V1SCom
089000020422     C                   eval      SpeCliBis = WSpeCli
089100020422      *
089200020422     C     KspeBis       setll     FnSpe01l
089300020422     C     kspeBis       reade     FnSpe01l                               32
089400020423   3aC     *in32         DOWEQ     *off
089500020422   4aC                   IF        SpeFlg <> 'A'
089600090218      * controllo se posso duplicare il luogo
089700090218     c                   exsr      ctrspe
089800090218      * luogo non in gestione alla filiale non duplico
089900090218   5ac                   if        $nospe = *off
090000020422     C                   eval      SpeCli = NumKsc
090100040719     C                   move      DATEU         spedtr
090200020422     C                   write     FnSpe000
090300060118      * scrivo anche Fnsp2
090400060118     c                   Eval      ksp2cli = wspecli
090500060118     c                   Eval      ksp2cod = specod
090600060118     c     KspeBis2      setll     FnSp201l
090700060118     c     kspeBis2      reade     FnSp201l                               33
090800060118    5c     *in33         DOWEQ     *off
090900060118    6c                   IF        Sp2Flg <> 'A'
091000060118     c                   eval      Sp2Cli = NumKsc
091100060118     c                   write     FnSp2000
091200060118    6c                   ENDIF
091300060118     c     kspeBis2      reade     FnSp201l                               33
091400060118    5c                   ENDDO
091500090218   5-c                   endif
091600020422   4-C                   ENDIF
091700020422     C     kspeBis       reade     FnSpe01l                               32
091800020422   3-C                   ENDDO
091900020430      *
092000020430      * Scrittura FnAcr
092100020430      * Valorizzo la chiave
092200020430     C                   eval      WAcrKsc = V1SKsc
092300020430     C                   eval      WAcrPoU = *zeros
092400020430     C                   eval      AcrCroBis = WAcrCro
092500020430      *
092600020430     C     AcrCroBis     setll     FnAcr01l
092700020430     C                   read      FnAcr01l                               32
092800020430   3aC     *in32         DOWEQ     *off
092900020430      *
093000020430      * Se ho già cambiato codice cliente esco dal ciclo
093100020430     C                   eval      WAcrCro = AcrCro
093200020430   4aC                   if        WAcrKsc <> v1sksc
093300020430     C                   leave
093400020430   4-C                   endif
093500020430      *
093600020430   4aC                   if        AcrAtb <> 'A'
093700020430      *
093800020430     C                   eval      WAcrCro = AcrCro
093900020430      * cod.bollettazione
094000020430   5aC                   if        WAcrKsc = Acrksc
094100020430     C                   move      NumKsc        AcrKsc
094200020430   5eC                   else
094300020430     C                   move      *zeros        AcrKsc
094400020430   5-C                   endif
094500020430      * cod
094600020430     C                   move      NumKsc        WAcrKsc
094700020430     C                   eval      AcrCro = WAcrCro
094800040910     C                   move      dateu         acrdtr
094900020430      * po di gestione
095000020430     C                   move      V1CPoU        acrpoa
095100020430     C                   write     FnAcr000
095200020430   4-C                   endif
095300020430     C                   read      FnAcr01l                               32
095400020430   3-C                   ENDDO
095500020422      *
095600111104      * Se richiesta emetto anche la stampa (1 x ogni cliente)
095700111104     c                   IF        *inkh
095800111104     c                   exsr      stampa
095900111104     c                   ENDIF
096000111104
096100020422   2-C                   ENDIF
096200111027
096300020417     C                   add       1             NRR
096400020422   1-C                   ENDDO
096500020418      *
096600020417     C                   EndSr
096700020422      *----------------------------------------------------------------
096800020422      *  Reperisco progressivo numero cliente
096900020422      *----------------------------------------------------------------
097000020422     C     Caric         BEGSR
097100020422     C                   z-add     0             NufAAA
097200020422     C                   z-add     DutKci        NufCnu
097300020422     C                   z-add     V1CPoU        NufFil
097400020422      *
097500020422     C     KNuf          chain     FlNuf                              21
097600020422      * Se non trovo il numeratore lo creo
097700020422   1aC   21              DO
097800020422     C                   z-add     0             NufNum
097900020422     C                   write     FlNuf
098000020422     C     KNuf          chain     FlNuf                              21
098100020422   1-C                   ENDDO
098200020422     C*
098300020422     C                   SETON                                        21
098400020422   1aC     *In21         DOWEQ     '1'
098500020422     C                   add       1             NufNum
098600020430   2aC                   IF        NufNum >= 10000
098700020422     C                   z-add     1             NufNum
098800020422   2-C                   END
098900020422      *
099000020423     C                   move      NufNum        KKsc
099100020423     C                   movel     V1CPoU        KKsc
099200020422     C     KAco          Setll     CnAco000                               21
099300020422   1-C                   ENDDO
099400020422      *
099500020422      * Codice cliente composto da P.O. + Numeratore
099600020423     C                   move      NufNum        NumKsc
099700020423     C                   movel     V1CPoU        NumKsc
099800020422      * AGGIORNO C O N T A T O R E   C L I E N T I
099900020422     C                   UPDATE    FlNuf
100000020422      *
100100020422     C                   ENDSR
100200130801      *
100300060915      *------------------------------------------------------------------------*
100400060915      * Scrittura file variazioni
100500060915      *------------------------------------------------------------------------*
100600060915     c     ScriviACV     BegSr
100700130801      *
100800060915     c                   Clear                   Tibs73ds
100900060918     c                   eval      ibs73pru = user
101000060918     c                   eval      ibs73noj = job_name
101100060918     c                   eval      ibs73pgm = 'TIBS35R'
101200060918     c                   eval      ibs73immag = 'D'
101300060918     c                   eval      ibs73cta = 'C'
101400060915     c                   call      'TIBS73R'
101500060915     c                   parm                    tibs73ds
101600060915     c                   parm                    dsaco
101700060915     c                   parm                    dsind
101800060915     c                   parm                    dsclp
101900060915     c                   parm                    dscls
102000130801      *
102100060915     c                   EndSr
102200130801      *
102300090218      *----------------------------------------------------------------
102400090218      *  Controllo luogo
102500090218      *----------------------------------------------------------------
102600090218     c     ctrspe        begsr
102700130801      *
102800090218     c                   eval      $nospe = *off
102900090218     c                   clear                   ds4l
103000090218     c                   eval      cod = '4L'
103100090218     c                   eval      key = specod
103200090218     c     ktab          chain     tabel00f
103300090218     c                   if        %found(tabel00f) and
103400090218     c                             tblflg = *Blanks
103500090218     c                   eval      ds4l = tbluni
103600090218     c                   endif
103700130801      *
103800090218      * se luogo in gestione solo dalla sede non posso duplicare
103900090218     c                   if        §4lufi <> 'S'
104000090218     c                   eval      $nospe = *on
104100090218     c                   endif
104200130801      *
104300090218     c                   endsr
104400130801      *
104500111027      *----------------------------------------------------------------
104600111027      *  Stampo abilitazioni
104700111027      *----------------------------------------------------------------
104800111027     c     Stampa        BEGSR
104900130801      *
105000111027     c                   clear                   tnta61ds
105100111027     c                   eval      TA61tla = 'P'
105200111027     c                   eval      TA61ksc = V1Sksc
105300111027     c                   eval      TA61newksc = Numksc
105400111027     c                   call      'TNTA61R'
105500111027     c                   parm                    kpjba
105600111027     c                   parm                    tnta61ds
105700130801      *
105800111027     c                   ENDSR
105900130801      *
106000020417      *----------------------------------------------------------------
106100020417      *  Reperisco Dati del job (Utente/Operativi)
106200020417      *----------------------------------------------------------------
106300020417     C     DatiJob       BegSr
106400020418      *
106500020418     C     *dtaara       define    §azute        azuteds
106600020418     C     *dtaara       define    §datiute      ddatiute
106700020418      *
106800020418     C                   in(E)     *dtaara
106900020418     C                   IF        %Error
107000020418     C                   call      'TIBS34R'
107100020418     C                   parm                    Tibs34Ds
107200020418     C                   in        *dtaara
107300020418     C                   ENDIF
107400020417      *
107500020513      *-- Verifica errori
107600020520     C                   IF        DUTERR = 'E'
107700020517     C                   movel     DUTMSG        V1CMsg
107800020520     C                   seton                                        9091
107900020513     C                   ENDIF
108000130801      *
108100050915      * Verifica errori e autorità profilo
108200050915      * se non c'è l'abilitazione
108300050915      * --> se 1° livello, abilitazioni al terminal
108400050915      *     se 2° livello, abilitazioni al punto operativo
108500050915      *     se sede, abilitazione azienda
108600050915     c                   Clear                   wabi
108700050915     c                   Clear                   dLat
108800050915if  1c                   If        UteAut = *Blanks
108900050915if  2c                   If        DutLpo = '1'
109000050915     c                   Eval      wabi   = 'TP'
109100050915e   2c                   EndIf
109200050915if  2c                   If        DutLpo = '2'
109300050915     c                   Eval      wabi   = 'PO'
109400050915e   2c                   EndIf
109500050915if  2c                   If        DutLpo = 'S'
109600050915     c                   Eval      wabi   = 'AZ'
109700050915e   2c                   EndIf
109800050915      * carica le abilitazioni del profilo
109900050915x   1c                   Else
110000050915     c                   Movel     UteFaf        Dute01
110100050915     c                   If        §UteCli <> *Blanks
110200050915     c                   Eval      wabi = §UteCli
110300050915     c                   Else
110400050915     c                   Eval      wabi = UteAut
110500050915     c                   EndIf
110600050915e   1c                   EndIf
110700130801      *
110800050915      * controllo se ok l'abilitazione dell'utente
110900050915     c                   Clear                   Tibs02ds
111000050915     c                   Eval      T02Mod = 'C'
111100050915     c                   Eval      T02Sif = knsif
111200050915     c                   Eval      T02Cod = 'LAT'
111300050915     c                   Movel(p)  wabi          T02Ke1
111400050915     c                   Call      'TIBS02R'
111500050915     c                   Parm                    kpjba
111600050915     c                   Parm                    Tibs02ds
111700050915if  1c                   If        T02Err = *Blanks
111800050915     c                   Eval      dLat = T02Uni
111900050915e   1c                   EndIf
112000050915      * errore o non abilitato
112100050915if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
112200050915     c                   Seton                                        9091
112300050915     C                   movel     msg(12)       v1cmsg
112400050915e   1c                   EndIf
112500130801      *
112600020418     C                   endsr
112700020422      *----------------------------------------------------------------
112800020422      *  Caricamento tabelle
112900020422      *----------------------------------------------------------------
113000020422     C     CarTab        BegSr
113100130801      *
113200050915      * Carico i p.o. in gestione all'utente in base all'autorizzazione specifica x i clienti
113300050915     c                   clear                   TRUL31DS
113400050915     c                   eval      I31abi = wabi
113500050915     c                   eval      I31cdi = DUTdis
113600050915     c                   eval      I31car = DUTare
113700050915     c                   eval      I31cpo = DUTpou
113800050915     c                   call      'TRUL31R'
113900050915     c                   parm                    KPJBA
114000050915     c                   parm                    TRUL31DS
114100050915if  1c                   if        O31pog > *zeros
114200050915     c                   movea     O31pog        pog
114300050915e   1c                   endif
114400130801      *
114500040719     C* GIRO UDATE
114600040719     C                   TIME                    W0140            14 0
114700040719     C                   MOVE      W0140         WDTGIO            8 0
114800040719     C                   Z-ADD     WDTGIO        G02DAT
114900040719     C                   MOVEL     *BLANK        G02ERR
115000040719     C                   CALL      'XSRDA8'
115100040719     C                   PARM                    WLBDAT
115200040719     C                   MOVEL     G02INV        DATEU             8 0
115201171129
115202171129       //Carico in sk le note che NON sono da duplicare
115203171129        clear IndiceNota;
115204171129        TBLkut = 1;
115205171129        TBLcod = '1T';
115206171129        setll (TBLkut:TBLcod) TABEL00F;
115207171129        reade (TBLkut:TBLcod) TABEL00F;
115208171129        DOW not %eof(TABEL00F);
115209171129          clear ds1T;
115210171129          IF  TBLflg = *blanks;
115211171129            ds1T = TBLuni;
115212171129          ENDIF;
115213171129          IF  §1Tdup <> 'S';
115214171129            IndiceNota += 1;
115215171129            skNote(IndiceNota) = TBLkey;
115217171129          ENDIF;
115218171129          reade (TBLkut:TBLcod) TABEL00F;
115219171129        ENDDO;
115220171129
115300020422      *
115400020422     C                   EndSr
115500020417      * ------------------------------------------------------------------
115600020418      * Klist
115700020422     C     KAco          KLIST
115800020422     C                   KFLD                    CodUt
115900020422     C                   KFLD                    KKcc
116000020422     C                   KFLD                    KKsc
116100020422      *
116200020422     C     KNtc          KLIST
116300020418     C                   KFLD                    NtcApl
116400020418     C                   KFLD                    NtcNk1
116500020418     C                   KFLD                    NtcNk2
116600020422      *
116700020422     C     KntcBis       KLIST
116800020422     C                   KFLD                    NtcAplBis
116900020422     C                   KFLD                    NtcNk1Bis
117000020422     C                   KFLD                    NtcNk2Bis
117100020422      *
117200020422     C     KSpe          KLIST
117300020418     C                   KFLD                    SpeFls
117400020418     C                   KFLD                    SpeCli
117500020422      *
117600020422     C     KSpeBis       KLIST
117700020426     C                   KFLD                    SpeFlsBis
117800020426     C                   KFLD                    SpeCliBis
117900060118      *
118000060118     C     KSpeBis2      KLIST
118100060118     C                   KFLD                    ksp2cli
118200060118     C                   KFLD                    ksp2cod
118300020422      *
118400020422     C     KTab          KLIST
118500020422     C                   KFLD                    CodUt
118600020422     C                   KFLD                    Cod
118700020422     C                   KFLD                    Key
118800020422      *
118900020422     C     KNuf          KLIST
119000020422     C                   KFLD                    NufAaa
119100020422     C                   KFLD                    NufCnu
119200020422     C                   KFLD                    NufFil
119300020417      * ------------------------------------------------------------------
119400130801** - Msg
119500111026Filiale obbligatoria                                                          1
119600111026Filiale errata o non in gestione all'utente                                   2
119700111026Opzione errata                                                                3
119800111026Cliente inesistente                                                           4
119900111026Indicare almeno un codice cliente da duplicare                                5
120000170306Duplica non consentita. Cod.Intestazione fattura in xxxxxxxxxxxxxxxxxxxxxxxxx 6
120100111026Codice commerciale non valido                                                 7
120200111026Codice commerciale obbligatorio                                               8
120300111026S.Conto intestazione fattura non corretto                                     9
120400111026I Codici clienti non possono essere ripetuti                                  10
120500111026Cliente scaduto                                                               11
120600050915Utente non abilitato alla funzione                                            12
120700051222Attenzione!!! Cliente bloccato con x. Enter x forzare                         13
120800051222Attenzione!!! xxxxxxxxxxxxxxxxxxxxxxxxx. Enter per forzare                    14
120900051223Attenzione! Il cliente sarà duplicato senza riportare l'esenzione IVA presente15
121000111026Duplica non consentita. Cliente con cod.pagamento fatt. gestibile solo da sede16
121100110715Duplica non consentita. Cliente con frequenza fattura settimanale!!           17
