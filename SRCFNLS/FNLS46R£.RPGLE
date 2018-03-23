000100001027     H DECEDIT('0,') DATEDIT(*DMY.)
000200110113      * FNLS46R *----------------------------------------------------------*
000300110113      *  Pgm di controllo  per cancellazione spunta
000400110113      *  al momento previsto solo l'ambito "QD"  cioè in quadratura distinta
000500080319      *--------------------------------------------------------------------*
000600080317     Ffnbrv07l  IF   E           K DISK
000700110114     Ffnbrv01l  uF   E           K DISK    rename(fnbrv000:fnbrv001)
000800080327     Ftabel00f  IF   E           K DISK
000900110114     Ffnbrve1l  IF a E           K DISK    RENAME(FNBRV000:FNBRVE1)
001000050705      *
001100110119     D MSG             S             78    DIM(12) CTDATA PERRCD(1)
001200080317      * - Passaggio parametri
001300080325     d KPJBA         e ds
001400110113     d FNLs46DS      e ds
001500080317     d FNLV53DS      e ds                  inz
001600080328     d
001700080328     d tibs34ds      e ds                  inz
001800080410     d tibs02ds      e ds                  inz
001900080328     d AzuteDs       e ds                  ExtName(AzUte00F) dtaara(§azute)
002000080328     d DDatiUte      e ds                  dtaara(§datiute)
002100110114
002200110114     D* PARAM2: PASSAGGIO AD ANNULLAMENTO ANOMALIE E DATE  - FNLSA8R -
002300110114     D PARAM2          DS
002400110114     D  WRVNFV                 2      6  0
002500110114     D  BRVNPG                 7      7  0
002600110114     D  BRVLNP                 8     10  0
002700110114     D  BRVLNA                11     13  0
002800110114     D  BRVNRS                14     15  0
002900110114     D  BRVNSC                16     22  0
003000110114     D  BRVCAN                23     23
003100110114     D  BRVVUC                24     30  6
003200110114     D  PARFLG                31     31
003300110114     D  PARFEL                92     94  0
003400110114     D  BRVNPS                95     96  0
003500110114     D  BRVFGS                97     99  0
003600110114     D  BRVFLE               100    102  0
003700110114     D  BRVpes               103    105  0
003800080317     d
003900080328     d ds7N          e ds
004000080328     d
004100080317     d BRVIMM          ds
004200080317     d brvdfs
004300080317     d brvhms
004400080327     d
004500110119     d A_dfv           s                   like(d53dfv)
004600110119     d B_dfv           s                   like(d53dfv)
004700110119     d A_spg           s                   like(d53spg)
004800110119     d B_spg           s                   like(d53spg)
004900110119     d A_IMM           s                   like(brvimm)
005000110119     d B_IMM           s                   like(brvimm)
005100110114     d Memnpg          s                   like(brvnpg)
005200110114     d Memspg          s                   like(d53spg)
005300110113
005400110113     d kkey            s                   like(tblkey)
005500080320     d knfv            s                   like(brvnfv)
005600110113     d kfgs            s                   like(brvfgs)
005700110113     d knpg            s                   like(brvnpg)
005800080320     d kfls            s                   like(brvlnp)
005900080320     d klna            s                   like(brvlna)
006000080320     d knrs            s                   like(brvnrs)
006100080320     d knsc            s                   like(brvnsc)
006200110114     d wpos            s              3  0
006300080317      *------------------------------------------------------------------------*
006400080317      *   P R O T O T I P I
006500080317      *------------------------------------------------------------------------*
006600081010      /copy gaitrasrc/srcprotopr,fnlv53r
006700081010      /copy gaitrasrc/srcprotopr,tibs34r
006800110114      /copy gaitrasrc/srcprotopr,fnlsa8r
006900110114
007000001027     C*---------------------------------------------------------------*
007100001027     C     *ENTRY        PLIST
007200080325     C                   PARM                    kpjba
007300110114     C                   PARM                    fnls46ds
007400080317      /free
007500110114
007600110113       clear  ols46err    ;
007700110113       clear  ols46msg    ;
007800110113       clear  ols46okc    ;
007900110113       clear  ols46ann    ;
008000080319
008100110113       //  Controllo di presenti spunte
0082001101131      if  ils46fls>0 and ils46nsc>0 and ils46lna>0 ;
008300110113
008400110113       kfls=ils46fls          ;
008500110113       klna=ils46lna          ;
008600110113       knrs=ils46nrs          ;
008700110113       knsc=ils46nsc          ;
008800110113
008900110113       // Per Ambito QD  si può cancellare solo cat 3 o 4
0090001101132      if  ils46amb='QD'   ;
009100110113
0092001101133      if ils46npgA<>3 and ils46npgA<>4  ;
009300110113       ols46err='I'   ;
009400110113       ols46msg=msg(2)  ;
0095001101133      endif  ;
0096001101193      if ils46npgB<>3 and ils46npgB<>4  ;
009700110113       ols46err='I'   ;
009800110119       ols46msg=msg(2)  ;
0099001101133      endif  ;
010000110113
0101001101193      if ils46nfvA =0 or ils46nfvB=0  ;
010200110113       ols46err='I'   ;
010300110113       ols46msg=msg(6)  ;
0104001101133      endif   ;
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
014900080320
015000080317        *inlr ='1'   ;
015100080327       //-------------------------------------------------------------------
015200080325       BEGSR  LeggiSpunte            ;
015300110113
015400110119       clear  A_dfv;
015500110119       clear  B_dfv;
015600110119       clear  A_spg;
015700110119       clear  B_spg;
015800110119       clear  A_imm;
015900110119       clear  B_imm;
016000110113
016100110113       // Spunta da Annullare
016200110113       knfv=ils46nfvA         ;
016300110114       knpg=ils46npgA         ;
016400110113       kfgs=ils46fgsA         ;
016500110114       chain(n) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
016600110113
016700110113 1     if not %found(fnbrv01l)   ;
016800110113         ols46err='I'   ;
016900110113         ols46msg=msg(1)  ;
017000110113         leavesr  ;
017100110113 1     endif    ;
017200110113
017300110113       // Prendo la data del foglio e la memorizzo
017400110113       clear  fnlv53DS ;
017500110113       d53nfv=brvnfv   ;
017600110113       d53npg=brvnpg   ;
017700110113       d53fgs=brvfgs   ;
017800110113       callp FNLV53R  (FNLV53DS)  ;
017900110113
0180001101131      if    d53err=' '           ;
018100110119        A_dfv=d53dfv  ;
018200110119        A_spg=d53spg ;
018300110113       else  ;
018400110119        A_dfv=brvdcs  ;
018500110119        clear  A_spg  ;
0186001101131      endif  ;
018700110119       A_imm=brvimm ;
018800110113
018900110113       // spunta presente
0190001101191      if ils46nfvB>0         ;
019100110113
019200110119       knfv=ils46nfvB         ;
019300110119       knpg=ils46npgB         ;
019400110119       kfgs=ils46fgsB         ;
019500110114       chain(n) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
019600110113
0197001101132      if not %found(fnbrv01l)   ;
019800110113         ols46err='I'   ;
019900110113         ols46msg=msg(4)  ;
020000110113         leavesr  ;
0201001101132      endif    ;
020200110113
020300110113       // Prendo la data del foglio e la memorizzo
020400110113       clear  fnlv53DS ;
020500110113       d53nfv=brvnfv   ;
020600110113       d53npg=brvnpg   ;
020700110113       d53fgs=brvfgs   ;
020800110113       callp FNLV53R  (FNLV53DS)  ;
020900110113
0210001101132      if    d53err=' '           ;
021100110119        B_dfv=d53dfv  ;
021200110119        B_spg=d53spg ;
021300110113       else  ;
021400110119        B_dfv=brvdcs  ;
021500110119        clear  B_spg  ;
0216001101132      endif  ;
021700110113
021800110119       B_imm=brvimm ;
021900080319
022000110113       // Per ambito  "QD" devono esere uguali le date
0221001101132      if ils46amb='QD'     ;
0222001101193      if A_dfv<>B_dfv   ;
022300110113         ols46err='I'   ;
022400110113         ols46msg=msg(5)  ;
022500110113         leavesr  ;
0226001101133      endif  ;
0227001101132      endif  ;
0228001101131      endif  ;
022900080319
023000080319       ENDSR   ;
023100110119       //-------------------------------------------------------------------
023200110119       BEGSR  Proc_QD  ;
023300110119
023400110119       // Se non trovati errori per ambito "QD" faccio
023500110119       //  CONTROLLI sulla data/ora immissione spunta
023600110119       //  in base all'automatismo di procedura
023700110119
0238001101192      if ols46err=' '   and ils46tela<>'A' ;
023900110119         EXSR  contrQD          ;
0240001101192      endif ;
024100110119
024200110119       // ANNULLAMENTO --> per tipo elaborazione "A" o "E" senza errori
0243001101192      if ols46err=' '                    ;
024400110119
024500110119 3       if ils46tela= 'A'   or ils46tela= 'E' ;
024600110119          knfv=ils46nfvA         ;
024700110119          knpg=ils46npgA         ;
024800110119          kfgs=ils46fgsA         ;
024900110119
025000110119           EXSR  CancellaSpu     ;
025100110119 3       endif ;
0252001101192      endif ;
025300110119
025400110119       ENDSR  ;
025500080327       //-------------------------------------------------------------------
025600110113       BEGSR  ContrQD         ;
025700110113       //  Se la spunta che si tiene   è > dell'altra e prevista
025800110113       //    dalla procedura --> ok
0259001101191      if  B_imm> A_Imm  ;
0260001101192         if ils46aupr='SE'  or %subst(ils46aupr:1:1)=%editc(ils46npgB:'X') ;
026100110113           ols46okc='S'   ;
026200110113           leavesr        ;
0263001101132         endif   ;
0264001101131      endif   ;
026500110113
026600110113       // Negli altri  casi devo segnalare l'errore
0267001101191      if B_imm<A_Imm;
026800110113        ols46msg=msg(7) ;
026900110113        ols46err='E'   ;
0270001101131      endif  ;
0271001101191      if B_imm=A_Imm;
027200110113        ols46msg=msg(8) ;
027300110113        ols46err='E'   ;
0274001101131      endif  ;
027500110113
027600110113       // Imposto bene messaggio
027700110113
0278001101191      if B_imm<=A_Imm;
027900110119          Memnpg=ils46npgB    ;
028000110119          Memspg=B_spg       ;
028100110114          wpos =69            ;
028200110114          exsr    Memomsg     ;
028300110114
028400110114          Memnpg=ils46npgA    ;
028500110119          Memspg=A_spg       ;
028600110114          wpos =28            ;
028700110114          exsr    Memomsg     ;
028800110113
028900110113x1       else    ;
029000110113
029100110114         // Se non è un problema di data è di procedura non prevista
029200110114
029300110114        ols46msg=msg(9) ;
029400110114        ols46err='E'   ;
029500110119          Memnpg=ils46npgB    ;
029600110119          Memspg=B_spg       ;
029700110114          wpos =32            ;
029800110114          exsr    Memomsg     ;
029900110114
030000110114          Memnpg=ils46npgA    ;
030100110119          Memspg=A_spg       ;
030200110114          wpos =61            ;
030300110114          exsr    Memomsg     ;
030400110114
030500110113 1       endif  ;
030600110113
030700110113       ENDSR    ;
030800110114       //-------------------------------------------------------------------
030900110114       BEGSR    MemoMSg   ;
031000110114
0311001101142      if Memnpg   =3  ;
031200110114          %subst(ols46msg:wpos:5)='I.M.' + Memspg  ;
031300110114x2     else ;
031400110114         clear kkey    ;
031500110114         kkey=%editc(memnpg:'X')  ;
031600110114         chain   (1:'7N':kkey) tabel00f   ;
0317001101143        if %found(tabel00f)   ;
031800110114           %subst(ols46msg:wpos:4)=tbluni   ;
0319001101143        endif   ;
0320001101142      endif   ;
032100110114c      ENDSR   ;
032200110113       //-------------------------------------------------------------------
032300110113       BEGSR  CancellaSpu     ;
032400110114
032500110114       // Alloco spunta da cancellare
032600110114       chain(e) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
032700110114       if %error ;
032800110114         ols46err='I'   ;
032900110114         ols46msg=msg(11) ;
033000110114         leavesr          ;
033100110114       endif   ;
033200110114
033300110114       // Cancello la spunta --> la metto in BRVE
033400110114       setll (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrve1l  ;
033500110114
033600110114       if not %equal(fnbrve1l)  ;
033700110114        write fnbrve1   ;
033800110114       endif  ;
033900110114
034000110114       // Eseguo aggiornamenti per la cancellazione della spunta
034100110121       //  non in quadratura perchè si incastrano le allocazioni
034200110121
034300110121       if  ils46amb<>'QD'   ;
034400110121        wrvnfv=brvnfv    ;
034500110121        parflg='1'       ;
034600110121        parfel=simfel    ;
034700110121        kpjbu=param2     ;
034800110121        callp FNLSA8R (KPJBA)   ;
034900110114
035000110121        param2=kpjbu    ;
035100110114
035200110121        if parflg='E'    ;
035300110121          ols46err='I'   ;
035400110121          ols46msg=msg(10) ;
035500110121          leavesr          ;
035600110121        endif   ;
035700110121       endif   ;
035800110114
035900110114       delete  fnbrv001  ;
036000110114
036100110114       ols46ann='S'  ;
036200110114
036300110113       ENDSR    ;
036400080327       //-------------------------------------------------------------------
036500080327       BEGSR  *INZSR                 ;
036600080328
036700080328       in(E) *dtaara   ;
036800080328       if    %error or rsut  =*blanks  ;
036900080328       callp TIBS34R (TIBS34DS)        ;
037000080328       in    *dtaara   ;
037100080328       endif                           ;
037200080328
037300080328       ENDSR                           ;
037400080317      /end-free
037500110113**
037600110119Prima spunta  non trovata
037700110119Categorie di spunta  per ambito "QD" devono essere "3" o "4"
037800110119
037900110119Seconda spunta   non trovata                                                   4
038000110119Spunta da annullare e spunta presente devono avere la stessa DATA              5
038100110119Manca il numero foglio spunta della prima o secoonda spunta                    6
038200110119Data/ora IMMISSIONE spunta XXXX. da annullare SUPERIORE alla spunta xxxx.      7
038300110114Data/ora IMMISSIONE spunta XXXX. da annullare   UGUALE  alla spunta xxxx.      8
038400110114ATTENZIONE! Controllare spunta xxxx.  SUPERIORE alla spunta xxxx.              9
038500110114Errori durante l'aggiornamento delle anomalie: riprovare !!                   10
038600110114Record spunta allocato: ritentare!!                                           11
038700110119Le spunte hanno stessa data/ora Immissione:impossibile stabilire la + recente 12
