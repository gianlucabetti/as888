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
004400130613     d
004500130613     d BRVSCA          ds
004600130613     d brvdcs
004700130613     d brvhcs
004800080327     d
004900110119     d A_dfv           s                   like(d53dfv)
005000110119     d B_dfv           s                   like(d53dfv)
005100110119     d A_spg           s                   like(d53spg)
005200110119     d B_spg           s                   like(d53spg)
005300110119     d A_IMM           s                   like(brvimm)
005400110119     d B_IMM           s                   like(brvimm)
005500110114     d Memnpg          s                   like(brvnpg)
005600110114     d Memspg          s                   like(d53spg)
005700110113
005800110113     d kkey            s                   like(tblkey)
005900080320     d knfv            s                   like(brvnfv)
006000110113     d kfgs            s                   like(brvfgs)
006100110113     d knpg            s                   like(brvnpg)
006200080320     d kfls            s                   like(brvlnp)
006300080320     d klna            s                   like(brvlna)
006400080320     d knrs            s                   like(brvnrs)
006500080320     d knsc            s                   like(brvnsc)
006600110114     d wpos            s              3  0
006700080317      *------------------------------------------------------------------------*
006800080317      *   P R O T O T I P I
006900080317      *------------------------------------------------------------------------*
007000081010      /copy gaitrasrc/srcprotopr,fnlv53r
007100081010      /copy gaitrasrc/srcprotopr,tibs34r
007200110114      /copy gaitrasrc/srcprotopr,fnlsa8r
007300110114
007400001027     C*---------------------------------------------------------------*
007500001027     C     *ENTRY        PLIST
007600080325     C                   PARM                    kpjba
007700110114     C                   PARM                    fnls46ds
007800080317      /free
007900110114
008000110113       clear  ols46err    ;
008100110113       clear  ols46msg    ;
008200110113       clear  ols46okc    ;
008300110113       clear  ols46ann    ;
008400080319
008500110113       //  Controllo di presenti spunte
0086001101131      if  ils46fls>0 and ils46nsc>0 and ils46lna>0 ;
008700110113
008800110113       kfls=ils46fls          ;
008900110113       klna=ils46lna          ;
009000110113       knrs=ils46nrs          ;
009100110113       knsc=ils46nsc          ;
009200110113
009300110113       // Per Ambito QD  si può cancellare solo cat 3 o 4
0094001101132      if  ils46amb='QD'   ;
009500110113
0096001101133      if ils46npgA<>3 and ils46npgA<>4  ;
009700110113       ols46err='I'   ;
009800110113       ols46msg=msg(2)  ;
0099001101133      endif  ;
0100001101193      if ils46npgB<>3 and ils46npgB<>4  ;
010100110113       ols46err='I'   ;
010200110119       ols46msg=msg(2)  ;
0103001101133      endif  ;
010400110113
0105001101193      if ils46nfvA =0 or ils46nfvB=0  ;
010600110113       ols46err='I'   ;
010700110113       ols46msg=msg(6)  ;
0108001101133      endif   ;
0109001101132      endif   ;
011000110113
0111001101132      if ols46err=' '  ;
011200110113         EXSR  Leggispunte      ;
0113001101132      endif ;
011400110119
011500110119       // Se tipo elaborazione "M" annullo la meno recente delle due
0116001101192      if ols46err=' ' ;
011700110119
0118001101193      if ils46tela= 'M'   ;
011900110119
012000110119       select ;
012100110119 4     when A_imm=B_imm   ;
012200110119         ols46err='I'   ;
012300110119         ols46msg=msg(12) ;
012400110119
012500110119 4     when A_imm<B_imm   ;
012600110119          knfv=ils46nfvA         ;
012700110119          knpg=ils46npgA         ;
012800110119          kfgs=ils46fgsA         ;
012900110119           EXSR  CancellaSpu     ;
013000110119
013100110119 4     when A_imm>B_imm   ;
013200110119          knfv=ils46nfvB         ;
013300110119          knpg=ils46npgB         ;
013400110119          kfgs=ils46fgsB         ;
013500110119           EXSR  CancellaSpu     ;
013600110119          endsl                  ;
013700110119
013800110119 x3      else  ;
013900110119
014000110119 4       if   ils46amb='QD'      ;
014100110119
014200110119           exsr Proc_QD   ;
014300110119
014400110119 4       endif ;
014500110119 3       endif ;
0146001101192      endif ;
014700110113
014800110113
014900110113x1     else  ;
015000110113       ols46err='I'   ;
015100110113       ols46msg=msg(1)  ;
0152001101131      endif                  ;
015300080320
015400080317        *inlr ='1'   ;
015500080327       //-------------------------------------------------------------------
015600080325       BEGSR  LeggiSpunte            ;
015700110113
015800110119       clear  A_dfv;
015900110119       clear  B_dfv;
016000110119       clear  A_spg;
016100110119       clear  B_spg;
016200110119       clear  A_imm;
016300110119       clear  B_imm;
016400110113
016500110113       // Spunta da Annullare
016600110113       knfv=ils46nfvA         ;
016700110114       knpg=ils46npgA         ;
016800110113       kfgs=ils46fgsA         ;
016900110114       chain(n) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
017000110113
017100110113 1     if not %found(fnbrv01l)   ;
017200110113         ols46err='I'   ;
017300110113         ols46msg=msg(1)  ;
017400110113         leavesr  ;
017500110113 1     endif    ;
017600110113
017700110113       // Prendo la data del foglio e la memorizzo
017800110113       clear  fnlv53DS ;
017900110113       d53nfv=brvnfv   ;
018000110113       d53npg=brvnpg   ;
018100110113       d53fgs=brvfgs   ;
018200110113       callp FNLV53R  (FNLV53DS)  ;
018300110113
0184001101131      if    d53err=' '           ;
018500110119        A_dfv=d53dfv  ;
018600110119        A_spg=d53spg ;
018700110113       else  ;
018800110119        A_dfv=brvdcs  ;
018900110119        clear  A_spg  ;
0190001101131      endif  ;
019100130613       if brvimm>brvsca and brvdcs>0;
019200130613       A_imm=brvsca ;
019300130613       else  ;
019400130613       A_imm=brvimm ;
019500130613       endif  ;
019600110113
019700110113       // spunta presente
0198001101191      if ils46nfvB>0         ;
019900110113
020000110119       knfv=ils46nfvB         ;
020100110119       knpg=ils46npgB         ;
020200110119       kfgs=ils46fgsB         ;
020300110114       chain(n) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
020400110113
0205001101132      if not %found(fnbrv01l)   ;
020600110113         ols46err='I'   ;
020700110113         ols46msg=msg(4)  ;
020800110113         leavesr  ;
0209001101132      endif    ;
021000110113
021100110113       // Prendo la data del foglio e la memorizzo
021200110113       clear  fnlv53DS ;
021300110113       d53nfv=brvnfv   ;
021400110113       d53npg=brvnpg   ;
021500110113       d53fgs=brvfgs   ;
021600110113       callp FNLV53R  (FNLV53DS)  ;
021700110113
0218001101132      if    d53err=' '           ;
021900110119        B_dfv=d53dfv  ;
022000110119        B_spg=d53spg ;
022100110113       else  ;
022200110119        B_dfv=brvdcs  ;
022300110119        clear  B_spg  ;
0224001101132      endif  ;
022500110113
022600130613       if brvimm>brvsca and brvdcs>0  ;
022700130613       B_imm=brvsca ;
022800130613       else  ;
022900110119       B_imm=brvimm ;
023000130613       endif  ;
023100080319
023200110113       // Per ambito  "QD" devono esere uguali le date
0233001101132      if ils46amb='QD'     ;
0234001101193      if A_dfv<>B_dfv   ;
023500110113         ols46err='I'   ;
023600110113         ols46msg=msg(5)  ;
023700110113         leavesr  ;
0238001101133      endif  ;
0239001101132      endif  ;
0240001101131      endif  ;
024100080319
024200080319       ENDSR   ;
024300110119       //-------------------------------------------------------------------
024400110119       BEGSR  Proc_QD  ;
024500110119
024600110119       // Se non trovati errori per ambito "QD" faccio
024700110119       //  CONTROLLI sulla data/ora immissione spunta
024800110119       //  in base all'automatismo di procedura
024900110119
0250001101192      if ols46err=' '   and ils46tela<>'A' ;
025100110119         EXSR  contrQD          ;
0252001101192      endif ;
025300110119
025400110119       // ANNULLAMENTO --> per tipo elaborazione "A" o "E" senza errori
0255001101192      if ols46err=' '                    ;
025600110119
025700110119 3       if ils46tela= 'A'   or ils46tela= 'E' ;
025800110119          knfv=ils46nfvA         ;
025900110119          knpg=ils46npgA         ;
026000110119          kfgs=ils46fgsA         ;
026100110119
026200110119           EXSR  CancellaSpu     ;
026300110119 3       endif ;
0264001101192      endif ;
026500110119
026600110119       ENDSR  ;
026700080327       //-------------------------------------------------------------------
026800110113       BEGSR  ContrQD         ;
026900110113       //  Se la spunta che si tiene   è > dell'altra e prevista
027000110113       //    dalla procedura --> ok
0271001101191      if  B_imm> A_Imm  ;
0272001101192         if ils46aupr='SE'  or %subst(ils46aupr:1:1)=%editc(ils46npgB:'X') ;
027300110113           ols46okc='S'   ;
027400110113           leavesr        ;
0275001101132         endif   ;
0276001101131      endif   ;
027700110113
027800110113       // Negli altri  casi devo segnalare l'errore
0279001101191      if B_imm<A_Imm;
028000110113        ols46msg=msg(7) ;
028100110113        ols46err='E'   ;
0282001101131      endif  ;
0283001101191      if B_imm=A_Imm;
028400110113        ols46msg=msg(8) ;
028500110113        ols46err='E'   ;
0286001101131      endif  ;
028700110113
028800110113       // Imposto bene messaggio
028900110113
0290001101191      if B_imm<=A_Imm;
029100110119          Memnpg=ils46npgB    ;
029200110119          Memspg=B_spg       ;
029300110114          wpos =69            ;
029400110114          exsr    Memomsg     ;
029500110114
029600110114          Memnpg=ils46npgA    ;
029700110119          Memspg=A_spg       ;
029800110114          wpos =28            ;
029900110114          exsr    Memomsg     ;
030000110113
030100110113x1       else    ;
030200110113
030300110114         // Se non è un problema di data è di procedura non prevista
030400110114
030500110114        ols46msg=msg(9) ;
030600110114        ols46err='E'   ;
030700110119          Memnpg=ils46npgB    ;
030800110119          Memspg=B_spg       ;
030900110114          wpos =32            ;
031000110114          exsr    Memomsg     ;
031100110114
031200110114          Memnpg=ils46npgA    ;
031300110119          Memspg=A_spg       ;
031400110114          wpos =61            ;
031500110114          exsr    Memomsg     ;
031600110114
031700110113 1       endif  ;
031800110113
031900110113       ENDSR    ;
032000110114       //-------------------------------------------------------------------
032100110114       BEGSR    MemoMSg   ;
032200110114
0323001101142      if Memnpg   =3  ;
032400110114          %subst(ols46msg:wpos:5)='I.M.' + Memspg  ;
032500110114x2     else ;
032600110114         clear kkey    ;
032700110114         kkey=%editc(memnpg:'X')  ;
032800110114         chain   (1:'7N':kkey) tabel00f   ;
0329001101143        if %found(tabel00f)   ;
033000110114           %subst(ols46msg:wpos:4)=tbluni   ;
0331001101143        endif   ;
0332001101142      endif   ;
033300110114c      ENDSR   ;
033400110113       //-------------------------------------------------------------------
033500110113       BEGSR  CancellaSpu     ;
033600110114
033700110114       // Alloco spunta da cancellare
033800110114       chain(e) (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrv01l  ;
033900110114       if %error ;
034000110114         ols46err='I'   ;
034100110114         ols46msg=msg(11) ;
034200110114         leavesr          ;
034300110114       endif   ;
034400110114
034500110114       // Cancello la spunta --> la metto in BRVE
034600110114       setll (knpg:knfv:kfgs:kfls:klna:knrs:knsc) fnbrve1l  ;
034700110114
034800110114       if not %equal(fnbrve1l)  ;
034900110114        write fnbrve1   ;
035000110114       endif  ;
035100110114
035200110114       // Eseguo aggiornamenti per la cancellazione della spunta
035300110121       //  non in quadratura perchè si incastrano le allocazioni
035400110121
035500110121       if  ils46amb<>'QD'   ;
035600110121        wrvnfv=brvnfv    ;
035700110121        parflg='1'       ;
035800110121        parfel=simfel    ;
035900110121        kpjbu=param2     ;
036000110121        callp FNLSA8R (KPJBA)   ;
036100110114
036200110121        param2=kpjbu    ;
036300110114
036400110121        if parflg='E'    ;
036500110121          ols46err='I'   ;
036600110121          ols46msg=msg(10) ;
036700110121          leavesr          ;
036800110121        endif   ;
036900110121       endif   ;
037000110114
037100110114       delete  fnbrv001  ;
037200110114
037300110114       ols46ann='S'  ;
037400110114
037500110113       ENDSR    ;
037600080327       //-------------------------------------------------------------------
037700080327       BEGSR  *INZSR                 ;
037800080328
037900080328       in(E) *dtaara   ;
038000080328       if    %error or rsut  =*blanks  ;
038100080328       callp TIBS34R (TIBS34DS)        ;
038200080328       in    *dtaara   ;
038300080328       endif                           ;
038400080328
038500080328       ENDSR                           ;
038600080317      /end-free
038700110113**
038800110119Prima spunta  non trovata
038900110119Categorie di spunta  per ambito "QD" devono essere "3" o "4"
039000110119
039100110119Seconda spunta   non trovata                                                   4
039200110119Spunta da annullare e spunta presente devono avere la stessa DATA              5
039300110119Manca il numero foglio spunta della prima o secoonda spunta                    6
039400110119Data/ora IMMISSIONE spunta XXXX. da annullare SUPERIORE alla spunta xxxx.      7
039500110114Data/ora IMMISSIONE spunta XXXX. da annullare   UGUALE  alla spunta xxxx.      8
039600110114ATTENZIONE! Controllare spunta xxxx.  SUPERIORE alla spunta xxxx.              9
039700110114Errori durante l'aggiornamento delle anomalie: riprovare !!                   10
039800110114Record spunta allocato: ritentare!!                                           11
039900110119Le spunte hanno stessa data/ora Immissione:impossibile stabilire la + recente 12
