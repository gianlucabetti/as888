000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200110113      * FNLS46R *----------------------------------------------------------*
000300110113      *  Pgm di controllo  per cancellazione spunta
000400110113      *  al momento previsto solo l'ambito "QD"  cioè in quadratura distinta
000500080319      *--------------------------------------------------------------------*
000600110114     Ffnbrv01l  uF   E           K DISK    rename(fnbrv000:fnbrv001)
000700110114     Ffnbrve1l  IF a E           K DISK    RENAME(FNBRV000:FNBRVE1)
000800050705      *
000900110119     D MSG             S             78    DIM(12) CTDATA PERRCD(1)
001000080317      * - Passaggio parametri
001100080325     d KPJBA         e ds
001200140611     d FNLs46DS1     e ds
001300080328     d
001400080328     d tibs34ds      e ds                  inz
001500080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
001600080328     d DDatiUte      e ds                  dtaara(§datiute)
001700110114
001800110114     D* PARAM2: PASSAGGIO AD ANNULLAMENTO ANOMALIE E DATE  - FNLSA8R -
001900110114     D PARAM2          DS
002000110114     D  WRVNFV                 2      6  0
002100110114     D  BRVNPG                 7      7  0
002200110114     D  BRVLNP                 8     10  0
002300110114     D  BRVLNA                11     13  0
002400110114     D  BRVNRS                14     15  0
002500110114     D  BRVNSC                16     22  0
002600110114     D  BRVCAN                23     23
002700110114     D  BRVVUC                24     30  6
002800110114     D  PARFLG                31     31
002900110114     D  PARFEL                92     94  0
003000110114     D  BRVNPS                95     96  0
003100110114     D  BRVFGS                97     99  0
003200110114     D  BRVFLE               100    102  0
003300110114     D  BRVpes               103    105  0
003400080328     d
003500080317     d BRVIMM          ds
003600080317     d brvdfs
003700080317     d brvhms
003800130613     d
003900130613     d BRVSCA          ds
004000130613     d brvdcs
004100130613     d brvhcs
004200080327     d
004300110119     d A_IMM           s                   like(brvimm)
004400110119     d B_IMM           s                   like(brvimm)
004500110114     d Memnpg          s                   like(brvnpg)
004600110113
004700080320     d knfv            s                   like(brvnfv)
004800110113     d kfgs            s                   like(brvfgs)
004900110113     d knpg            s                   like(brvnpg)
005000080320     d kfls            s                   like(brvlnp)
005100080320     d klna            s                   like(brvlna)
005200080320     d knrs            s                   like(brvnrs)
005300080320     d knsc            s                   like(brvnsc)
005400110114     d wpos            s              3  0
005500080317      *------------------------------------------------------------------------*
005600080317      *   P R O T O T I P I
005700080317      *------------------------------------------------------------------------*
005800081010      /copy gaitrasrc/srcprotopr,tibs34r
005900140612      // /copy gaitrasrc/srcprotopr,fnlsa8r
006000110114
006100001027     C*---------------------------------------------------------------*
006200001027     C     *ENTRY        PLIST
006300080325     C                   PARM                    kpjba
006400140611     C                   PARM                    fnls46ds1
006500080317      /free
006600110114
006700110113       clear  ols46err    ;
006800110113       clear  ols46msg    ;
006900110113       clear  ols46okc    ;
007000110113       clear  ols46ann    ;
007100080319
0072001406110      if   ils46tla<>'C'  ;
007300140611
007400110113       //  Controllo di presenti spunte
0075001101131      if  ils46fls>0 and ils46nsc>0 and ils46lna>0 ;
007600110113
007700110113       kfls=ils46fls          ;
007800110113       klna=ils46lna          ;
007900110113       knrs=ils46nrs          ;
008000110113       knsc=ils46nsc          ;
008100110113
008200110113       // Per Ambito QD  si può cancellare solo cat 3 o 4
0083001101132      if  ils46amb='QD'   ;
008400110113
0085001101133      if ils46npgA<>3 and ils46npgA<>4  ;
008600110113       ols46err='I'   ;
008700110113       ols46msg=msg(2)  ;
0088001101133      endif  ;
0089001101193      if ils46npgB<>3 and ils46npgB<>4  ;
009000110113       ols46err='I'   ;
009100110119       ols46msg=msg(2)  ;
0092001101133      endif  ;
009300110113
0094001101193      if ils46nfvA =0 or ils46nfvB=0  ;
009500110113       ols46err='I'   ;
009600110113       ols46msg=msg(6)  ;
0097001101133      endif   ;
009800140611
009900140611       // Per ambito  "QD" le date devono esere uguali le date
0100001406113      if ILS46DFVA<>ILS46DFVB  ;
010100140611         ols46err='I'   ;
010200140611         ols46msg=msg(5)  ;
0103001406113      endif  ;
010400140611
0105001101132      endif   ;
010600110113
0107001101132      if ols46err=' '  ;
010800110113         EXSR  Leggispunte      ;
0109001101132      endif ;
011000110119
011100110119       // Se tipo elaborazione "M" annullo la meno recente delle due
0112001101192      if ols46err=' ' ;
011300110119
0114001101193      if ils46tela= 'M'   ;
011500110119
011600110119       select ;
011700110119 4     when A_imm=B_imm   ;
011800110119         ols46err='I'   ;
011900110119         ols46msg=msg(12) ;
012000110119
012100110119 4     when A_imm<B_imm   ;
012200110119          knfv=ils46nfvA         ;
012300110119          knpg=ils46npgA         ;
012400110119          kfgs=ils46fgsA         ;
012500110119           EXSR  CancellaSpu     ;
012600110119
012700110119 4     when A_imm>B_imm   ;
012800110119          knfv=ils46nfvB         ;
012900110119          knpg=ils46npgB         ;
013000110119          kfgs=ils46fgsB         ;
013100110119           EXSR  CancellaSpu     ;
013200110119          endsl                  ;
013300110119
013400110119 x3      else  ;
013500110119
013600110119 4       if   ils46amb='QD'      ;
013700110119
013800110119           exsr Proc_QD   ;
013900110119
014000110119 4       endif ;
014100110119 3       endif ;
0142001101192      endif ;
014300110113
014400110113
014500110113x1     else  ;
014600110113       ols46err='I'   ;
014700110113       ols46msg=msg(1)  ;
0148001101131      endif                  ;
0149001406110      endif                  ;
015000080320
015100140611       if ils46tla=' ' ;
015200140611        *inrt ='1'   ;
015300140611       else   ;
015400140611
015500140612       //if  ils46amb<>'QD'   ;
015600140612       //clear param2;
015700140612       //parflg='C'  ;
015800140612       // kpjbu=param2     ;
015900140612       // callp FNLSA8R (KPJBA)   ;
016000140612       // endif  ;
016100140611
016200080317        *inlr ='1'   ;
016300140611        endif  ;
016400080327       //-------------------------------------------------------------------
016500080325       BEGSR  LeggiSpunte            ;
016600110113
016700110119       clear  A_imm;
016800110119       clear  B_imm;
016900110113
017000110113       // Spunta da Annullare
017100110113       knfv=ils46nfvA         ;
017200110114       knpg=ils46npgA         ;
017300110113       kfgs=ils46fgsA         ;
017400110114       chain(n) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
017500110113
017600110113 1     if not %found(fnbrv01l)   ;
017700110113         ols46err='I'   ;
017800110113         ols46msg=msg(1)  ;
017900110113         leavesr  ;
018000110113 1     endif    ;
018100110113
018200130613       if brvimm>brvsca and brvdcs>0;
018300130613       A_imm=brvsca ;
018400130613       else  ;
018500130613       A_imm=brvimm ;
018600130613       endif  ;
018700110113
018800110113       // spunta presente
0189001101191      if ils46nfvB>0         ;
019000110113
019100110119       knfv=ils46nfvB         ;
019200110119       knpg=ils46npgB         ;
019300110119       kfgs=ils46fgsB         ;
019400110114       chain(n) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
019500110113
0196001101132      if not %found(fnbrv01l)   ;
019700110113         ols46err='I'   ;
019800110113         ols46msg=msg(4)  ;
019900110113         leavesr  ;
0200001101132      endif    ;
020100110113
020200130613       if brvimm>brvsca and brvdcs>0  ;
020300130613       B_imm=brvsca ;
020400130613       else  ;
020500110119       B_imm=brvimm ;
020600130613       endif  ;
020700080319
0208001101131      endif  ;
020900080319
021000080319       ENDSR   ;
021100110119       //-------------------------------------------------------------------
021200110119       BEGSR  Proc_QD  ;
021300110119
021400110119       // Se non trovati errori per ambito "QD" faccio
021500110119       //  CONTROLLI sulla data/ora immissione spunta
021600110119       //  in base all'automatismo di procedura
021700110119
0218001101192      if ols46err=' '   and ils46tela<>'A' ;
021900110119         EXSR  contrQD          ;
0220001101192      endif ;
022100110119
022200110119       // ANNULLAMENTO --> per tipo elaborazione "A" o "E" senza errori
0223001101192      if ols46err=' '                    ;
022400110119
022500110119 3       if ils46tela= 'A'   or ils46tela= 'E' ;
022600110119          knfv=ils46nfvA         ;
022700110119          knpg=ils46npgA         ;
022800110119          kfgs=ils46fgsA         ;
022900110119
023000110119           EXSR  CancellaSpu     ;
023100110119 3       endif ;
0232001101192      endif ;
023300110119
023400110119       ENDSR  ;
023500080327       //-------------------------------------------------------------------
023600110113       BEGSR  ContrQD         ;
023700110113       //  Se la spunta che si tiene   è > dell'altra e prevista
023800110113       //    dalla procedura --> ok
0239001101191      if  B_imm> A_Imm  ;
0240001101192         if ils46aupr='SE'  or %subst(ils46aupr:1:1)=%editc(ils46npgB:'X') ;
024100110113           ols46okc='S'   ;
024200110113           leavesr        ;
0243001101132         endif   ;
0244001101131      endif   ;
024500110113
024600110113       // Negli altri  casi devo segnalare l'errore
0247001101191      if B_imm<A_Imm;
024800110113        ols46msg=msg(7) ;
024900110113        ols46err='E'   ;
0250001101131      endif  ;
0251001101191      if B_imm=A_Imm;
025200110113        ols46msg=msg(8) ;
025300110113        ols46err='E'   ;
0254001101131      endif  ;
025500110113
025600110113       // Imposto bene messaggio
025700110113
0258001101191      if B_imm<=A_Imm;
025900110119          Memnpg=ils46npgB    ;
026000110114          wpos =69            ;
026100110114          exsr    Memomsg     ;
026200110114
026300110114          Memnpg=ils46npgA    ;
026400110114          wpos =28            ;
026500110114          exsr    Memomsg     ;
026600110113
026700110113x1       else    ;
026800110113
026900110114         // Se non è un problema di data è di procedura non prevista
027000110114
027100110114        ols46msg=msg(9) ;
027200110114        ols46err='E'   ;
027300110119          Memnpg=ils46npgB    ;
027400110114          wpos =32            ;
027500110114          exsr    Memomsg     ;
027600110114
027700110114          Memnpg=ils46npgA    ;
027800110114          wpos =61            ;
027900110114          exsr    Memomsg     ;
028000110114
028100110113 1       endif  ;
028200110113
028300110113       ENDSR    ;
028400110114       //-------------------------------------------------------------------
028500110114       BEGSR    MemoMSg   ;
028600110114
0287001101142      if Memnpg   =3  ;
028800140611          %subst(ols46msg:wpos:5)='I.M.A.'         ;
028900140611x2     else ;
029000140611           %subst(ols46msg:wpos:4)='Cons'   ;
0291001101142      endif   ;
029200110114c      ENDSR   ;
029300110113       //-------------------------------------------------------------------
029400110113       BEGSR  CancellaSpu     ;
029500110114
029600110114       // Alloco spunta da cancellare
029700110114       chain(e) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
029800110114       if %error ;
029900110114         ols46err='I'   ;
030000110114         ols46msg=msg(11) ;
030100110114         leavesr          ;
030200110114       endif   ;
030300110114
030400110114       // Cancello la spunta --> la metto in BRVE
030500110114       setll (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrve1l  ;
030600110114
030700110114       if not %equal(fnbrve1l)  ;
030800110114        write fnbrve1   ;
030900110114       endif  ;
031000110114
031100110114       // Eseguo aggiornamenti per la cancellazione della spunta
031200110121       //  non in quadratura perchè si incastrano le allocazioni
031300110121
031400140612       //if  ils46amb<>'QD'   ;
031500140612       // wrvnfv=brvnfv    ;
031600140612       // parflg='1'       ;
031700140612       // parfel=simfel    ;
031800140612       // kpjbu=param2     ;
031900140612       // callp FNLSA8R (KPJBA)   ;
032000110114
032100140612       // param2=kpjbu    ;
032200110114
032300140612       // if parflg='E'    ;
032400140612       //   ols46err='I'   ;
032500140612       //   ols46msg=msg(10) ;
032600140612       //   leavesr          ;
032700140612       // endif   ;
032800140612       //endif   ;
032900110114
033000110114       delete  fnbrv001  ;
033100110114
033200110114       ols46ann='S'  ;
033300110114
033400110113       ENDSR    ;
033500080327       //-------------------------------------------------------------------
033600080327       BEGSR  *INZSR                 ;
033700080328
033800080328       in(E) *dtaara   ;
033900080328       if    %error or rsut  =*blanks  ;
034000080328       callp TIBS34R (TIBS34DS)        ;
034100080328       in    *dtaara   ;
034200080328       endif                           ;
034300080328
034400080328       ENDSR                           ;
034500080317      /end-free
034600110113**
034700110119Prima spunta  non trovata
034800110119Categorie di spunta  per ambito "QD" devono essere "3" o "4"
034900110119
035000110119Seconda spunta   non trovata                                                   4
035100110119Spunta da annullare e spunta presente devono avere la stessa DATA              5
035200140611Manca il numero foglio spunta della prima o seconda spunta                     6
035300110119Data/ora IMMISSIONE spunta XXXX. da annullare SUPERIORE alla spunta xxxx.      7
035400110114Data/ora IMMISSIONE spunta XXXX. da annullare   UGUALE  alla spunta xxxx.      8
035500110114ATTENZIONE! Controllare spunta xxxx.  SUPERIORE alla spunta xxxx.              9
035600110114Errori durante l'aggiornamento delle anomalie: riprovare !!                   10
035700110114Record spunta allocato: ritentare!!                                           11
035800110119Le spunte hanno stessa data/ora Immissione:impossibile stabilire la + recente 12
