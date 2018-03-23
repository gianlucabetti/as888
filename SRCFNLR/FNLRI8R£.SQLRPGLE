000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000400160316      *  Nome programma:  FNLRI7R
000500160316      *  Descrizione   :  Manutenzione Vedi Pacco DPD
000600160316      *  Data creazione:  16 MAR 2016
000700940712      *------------------------------------------------------------------------*
000800991105     H DECEDIT('0,') DATEDIT(*DMY/)
000900160323     h dftactgrp(*no) actgrp(*caller)
001000940712      *
001100160317     FFNLRI7D   CF   E             WORKSTN
001200040218     FAZORG01L  IF   E           K DISK
001300940712     F*.......... B O L L E   A R R I V I
001400940712     FFNARB01L  IF   E           K DISK
001500170926     FFipnd01L  IF   E           K DISK
001600170926     FFipnd03L  IF   E           K DISK
001700170926     f                                     rename(fipnd000:fipnd03)
001800940712     F*.......... D E T T A G L I O   C O L L I
001900160316     FFNART27L  IF   E           K DISK
002000040218      *------------------------------------------------------------------------*
002100060130     D $pog            s              3    dim(250)
002200060130     d
002300160323     D err             S             78    DIM(35) CTDATA PERRCD(1)
002400911024     D*
002500160316     D                 DS
002600160316     D  V1CBCODEB              1     18  0
002700160316     D  wbcodfls               1      3  0
002800160316     D  wbcodlna               4      6  0
002900160316     D  wbcodnrs               7      8  0
003000160316     D  wbcodnsc               9     15  0
003100160317
003200160317     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
003300160317     D PARAM1          DS                  INZ
003400160317     D  PA1AAS                14     17  0
003500160317     D  PA1LNP                18     20  0
003600160317     D  PA1NRS                21     22  0
003700160317     D  PA1NSP                23     29  0
003800160317     D  PA1F03                30     30
003900160317     D  PA1FLG                31     31
004000160317     D  PA1PGM                32     41
004100160317     D  PA1DSA                42     61
004200160317     D  PA1CML                62     62
004300160317     D  PA1DSP                63     70  0
004400160317     D  PA1RMN                71     85  0
004500160317     D  PA1LNA                86     88  0
004600160317     D  PA1LN1                89    108
004700160317     D  PA1RSD               109    143
004800160317     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
004900160317     D  PA1GIA               144    144
005000160317     D  PA1FEL               145    147  0
005100160317     D  PA1TBO               148    148
005200040218      *
005300040218     d Status         sds
005400040218     d  SDSprm           *parms
005500160317     D  N_pgm                  1      8
005600991027      *
005700040216     D KPJBA         E DS
005800040225      *
005900040216      *
006000170926     D** DSBL4I        E DS
006100170926
006200170926     d DPNDFLO       E DS
006300941014      *
006400941212     D* DS PER TRUL06R - CARICAMENTO £X
006500991027     D TRUL06DS      E DS
006600160321     D  lint                   1     90  0 DIM(30)
006700941212      *
006800941014     D* DS PER FNLR48R - MANUTENZIONE BOLLE ARRIVI
006900991027     D FNLR48DS      E DS
007000160317     d fnlv55ds      e ds                  inz
007100160317     d trul90ds      e ds                  inz
007200160317     d dsarbd        e ds                  inz
007300160317     d dsarbk        e ds                  inz
007400160317     d darbt         e ds                  inz
007500160317     d dsarbg        e ds                  inz
007600160317     d dsarbn        e ds                  inz
007700941014      *
007800160317     d Tibs02Ds      e ds
007900060130     d Azuteds       e ds                  extname(Azute00f)
008000060130     d dDatiute      e ds
008100060130     d dLat          e ds
008200060130     d dute01        e ds
008300060130     D TRUL31DS      E DS
008400060130     D TRUL31DS2     E DS
008500060130     D TIBS34DS      E DS
008600160317     D og143         E DS
008700060130     d wabi            s                   like(UteAut)
008800170926     d** krpe            s                   like(ar4n30)
008900170926     d kipn            s                   like(pndipn)
009000160317     d comdta          s                   like(v1cdta)
009100160317     d comdtd          s                   like(v1cdtd)
009200030703
009300160317     d Dataiso         s               d   datfmt(*iso)
009400160317     d Dataeur         s               d   datfmt(*eur)
009500160316     d pos             s              2  0
009600160317     d contP           s              2  0
009700160317     d errbolla        s              1
009800170926     d** wesci           s              1
009900160317     d wsel1           s              1  0
010000160317     d wsel2           s              1  0
010100160317     d wsel3           s              1  0
010200170926     d S_AR4AAS        s                   LIKE(pndAAS)
010300170926     d S_AR4LNP        s                   LIKE(pndLNP)
010400170926     d S_AR4NRS        s                   LIKE(pndNRS)
010500170926     d S_AR4NSP        s                   LIKE(pndNSP)
010600131111     d
010700991027      *
010800160316      * - Flags booleani
010900160316     d $Fine           s               n   inz(*off)
011000160316     d $InzD01         s               n   inz(*on)
011100160316     D digits          c                   '0123456789'
011200160317
011300160317       //--------------------------------------------------------------
011400160317       //?Definizione procedure.
011500160317       //--------------------------------------------------------------
011600160317
011700160317     D fnlv55r         PR
011800160317     D                                     extpgm('FNLV55R')
011900160317     D fnlv55ds                            likeds(fnlv55ds)
012000160317     D fnlr36r         PR
012100160317     D                                     extpgm('FNLR36R')
012200160317     D kpjba                               likeds(kpjba)
012300160317      // ? PROTOTIPI ?
012400160317      /copy gaitrasrc/srcprotopr,fnlr48r
012500900518     C****************************************************************
012600900518     C*  RIEPILOGO INDICATORI
012700900518     C***************************************************************
012800131111     C* 40-59 - ERRORI
012900920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
013000900518     C*****************************************************************
013100000000     C     *ENTRY        PLIST
013200000000     C                   PARM                    KPJBA
013300131210     c
013400160316     C                   Z-ADD     1             CODUT             1 0
013500160316     c                   Movel     rsut          V1cdsa
013600131210      /free
013700131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013800160316
013900160316       //?Gestione video
014000160316       DOW $Fine = *off;
014100160316             exsr GesD01;
014200160316       ENDDO;
014300160316
014400160316       eval *inlr=*on;
014500160316       //-------------------------------------------------------------*
014600160316       //                                                             *
014700160316       //-------------------------------------------------------------*
014800160316       Begsr Gesd01;
014900160316       // Inizializzazione videata
015000160316          if $inzd01;
015100160317              clear v1clp2 ;
015200160317              clear v1cnrs ;
015300160317              clear v1cnsp ;
015400160317              clear v1cnscs;
015500160317              clear v1cfls ;
015600160317              clear v1cdtd ;
015700160317              clear v1cdta ;
015800160317              clear v1clna ;
015900160317              clear v1cdea ;
016000160317              clear v1cnrd ;
016100160317              clear v1cbcodeb;
016200160317              clear v1clnas  ;
016300160317      /end-free
016400160317     C                   z-add     UYEAR         V1CAAS
016500160317     c     uyear         iflt      60
016600160317     c                   movel     20            v1caas
016700160317     c                   else
016800160317     C                   MOVEL     19            V1CAAS
016900160317     c                   endif
017000160317      /free
017100160316              $InzD01  = *off;
017200160316          endif;
017300160316             exfmt lri7d01;
017400160316             clear v1cmsg;
017500160316             *in28=*off;
017600160316             *in90=*off;
017700160316             select;
017800160316             // F3=Fine
017900160316                when *inkc;
018000160316                   $fine=*on;
018100160316                   leavesr;
018200160317             // F7=Int. Bolle
018300160317                when *inkg;
018400160317                 clear  kpjbu ;
018500160317                 clear  param1;
018600160317                 PA1AAS=v1caas;
018700160317                 PA1LNP=v1clp2;
018800160317                 PA1NRS=v1cnrs;
018900160317                 PA1NSP=v1cnsp;
019000160317                 if pa1nsp=0;
019100160317                    PA1FLG='2' ;
019200160317                 else;
019300160317                    PA1FLG='1' ;
019400160317                 endif;
019500160317                 clear  PA1CML;
019600160317                 PA1PGM = N_pgm  ;
019700160317                 kpjbu=param1;
019800160317                 FNLR36R(kpjba)  ;
019900160317                 param1=kpjbu;
020000160317                 if pa1nsp>0;
020100160317                    v1caas=pa1aas ;
020200160317                    v1clp2=pa1lnp ;
020300160317                    v1cnrs=pa1nrs ;
020400160317                    v1cnsp=pa1nsp;
020500160317                 endif;
020600160317                 leavesr;
020700160316                other;
020800160316                   exsr ctrd01;
020900160316                   if *in90;
021000160316                      leavesr;
021100160316                   endif;
021200160317                      exsr sr_lr48;
021300160401       //             if d48err=*blanks;
021400160331                         $inzd01=*on;
021500160401       //             else ;
021600160401       //                v1cmsg=err(15);
021700160401       //                *in28=*on;
021800160401       //             endif;
021900160318                      leavesr;
022000160316             endsl;
022100160316       endsr;
022200160316       //--------------------------------------------------------------
022300160316       //?Controlli videata
022400160316       //--------------------------------------------------------------
022500160316       BEGSR ctrd01         ;
022600160316          *in41=*off;
022700160316          *in42=*off;
022800160316          *in43=*off;
022900160316          *in44=*off;
023000160316          *in45=*off;
023100160316          *in46=*off;
023200160316          *in51=*off;
023300160316          *in52=*off;
023400160316          *in55=*off;
023500160316          wsel1=0      ;
023600160316          wsel2=0      ;
023700160316          wsel3=0      ;
023800160316       //?Barcode Bartolini letto da pistola OPPURE digitato a mano
023900160316       if v1cbcodeb>0 and (v1cfls>0 or v1clnas>0 or v1cnscs>0);
024000160316                *in28=*on;
024100160316                *in90=*on;
024200160316                *in42=*on;
024300160316                v1cmsg=err(1);
024400160316                leavesr;
024500160316       endif;
024600160316       //?Barcode Bartolini  (lettura  con pistola)
024700160316       if   V1CBCODEB>0    ;
024800160316          chain (wbcodfls:wbcodlna:wbcodnrs:wbcodnsc) fnart27l    ;
024900160316          if not %found(fnart27l);
025000160316                *in28=*on;
025100160316                *in90=*on;
025200160316                *in42=*on;
025300160316                v1cmsg=err(34);
025400160316                leavesr;
025500160316          endif;
025600160316          wsel1=1  ;
025700160316          chain (artaas:artlnp:artnrs:artnsp) fnarb01l;
025800160316          exsr ctrbolla;
025900160316          if errbolla='1';
026000160316             *in42=*on;
026100160316             leavesr;
026200160316          endif;
026300160316       endif;
026400160316       //?Segnacollo Bartolini (digitato a mano)
026500160316       if   v1cfls>0 or v1clnas>0 or v1cnscs>0;
026600160316       // Filiale segnacollo
026700160316          chain v1cfls azorg01l;
026800160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
026900160316                                      orgfva<>*blanks      ;
027000160316                *in28=*on;
027100160316                *in90=*on;
027200160316                *in43=*on;
027300160316                v1cmsg=err(35);
027400160316                leavesr;
027500160316          endif ;
027600160316       // linea di arrivo
027700160316          chain v1clnas azorg01l;
027800160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
027900160316                                      orgfva<>*blanks      ;
028000160316                *in28=*on;
028100160316                *in90=*on;
028200160316                *in44=*on;
028300160316                v1cmsg=err(35);
028400160316                leavesr;
028500160316          endif ;
028600160316          chain (v1cfls:v1clnas:v1cnrss:v1cnscs) fnart27l    ;
028700160316          if not %found(fnart27l);
028800160316                *in28=*on;
028900160316                *in90=*on;
029000160316                *in43=*on;
029100160316                v1cmsg=err(34);
029200160316                leavesr;
029300160316          endif;
029400160316          wsel1=1   ;
029500160316          chain (artaas:artlnp:artnrs:artnsp) fnarb01l;
029600160316          exsr ctrbolla;
029700160316          if errbolla='1';
029800160316             *in43=*on;
029900160316             leavesr;
030000160316          endif;
030100160316       endif;
030200160316       //?Parcel DPD
030300160316       if    v1cnrd<>*blanks;
030400160316             if %scan(' ':v1cnrd)>0 ;
030500160316                *in28=*on;
030600160316                *in90=*on;
030700160316                *in41=*on;
030800160316                v1cmsg=err(33);
030900160316                leavesr;
031000160316             endif;
031100160316       // Controllo se contiene solo numeri
031200160316       // Possono esserci lettere solo nei primi 4 byte. Dal quinto in poi devono
031300160316       // essere solo numeri
031400160316             pos=%check(digits:v1cnrd);
031500160316             if pos>4;
031600160316                *in28=*on;
031700160316                *in90=*on;
031800160316                *in41=*on;
031900160316                v1cmsg=err(33);
032000160316                leavesr;
032100160316             endif;
032200160316       // Cerco la spedizione
032300170926       //   krpe=v1cnrd;
032400170926          kipn=v1cnrd;
032500160316          contp=0;
032600160330          CLEAR s_ar4aas       ;
032700160330          CLEAR s_ar4LNP       ;
032800160330          CLEAR s_ar4NRS       ;
032900160330          CLEAR s_ar4NSP       ;
033000170926
033100170926          // wesci=*blanks;
033200170926          //setll ('I':krpe) fiar403l  ;
033300170926          //reade 'I' fiar403l;
033400170926          //dow not %eof(fiar403l) and wesci=*blanks ;
033500170926
033600170926            setll  kipn   fipnd03l  ;
033700170926            reade  kipn   fipnd03l  ;
033800170926            dow not %eof(fipnd03l)  ;
033900170926
034000170926          //   dsbl4i=ar4n30;
034100170926          // if  §b4ipn= v1cnrd;
034200170926                s_ar4aas=pndAAS;
034300170926                s_ar4LNP=pndLNP;
034400170926                s_ar4NRS=pndNRS;
034500170926                s_ar4NSP=pndNSP;
034600160316                contP+=1;
034700170926          // else;
034800170926          //    wesci='1';
034900170926          // endif;
035000170926       //    if wesci = *blanks;
035100170926       //       reade 'I' fiar403l;
035200170926       //    endif;
035300170926
035400170926            reade  kipn   fipnd03l  ;
035500160316          enddo;
035600170926
035700160316       // Errore se presente più di una spedizione con lo stesso numero parcel
035800160316          if contP>1;
035900160316                *in28=*on;
036000160316                *in90=*on;
036100160316                *in41=*on;
036200160316                v1cmsg=err(12);
036300160316             leavesr;
036400160316          endif;
036500160316       // Errore se non trovata neanche una spedizione
036600160316          if contP=0;
036700160316                *in28=*on;
036800160316                *in90=*on;
036900160316                *in41=*on;
037000160316                v1cmsg=err(11);
037100160316             leavesr;
037200160316          endif;
037300160316
037400160330          wsel2=1  ;
037500160330          chain (S_Ar4aas:S_ar4lnp:S_ar4nrs:S_ar4nsp) fnarb01l;
037600160316          exsr ctrbolla;
037700160316          if errbolla='1';
037800160316             *in41=*on;
037900160316             leavesr;
038000160316          endif;
038100160316       endif;
038200170926
038300160316       //?Key spedizione
038400160316       if   v1clp2>0 or v1cnsp>0;
038500160316          chain v1clp2 azorg01l;
038600160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
038700160316                                      orgfva<>*blanks      ;
038800160316                *in28=*on;
038900160316                *in90=*on;
039000160316                *in45=*on;
039100160316                v1cmsg=err(5);
039200160316                leavesr;
039300160316          endif ;
039400160316          og143=orgde3;
039500160316          if §ogntw<>'DPD';
039600160316                *in28=*on;
039700160316                *in90=*on;
039800160316                *in45=*on;
039900160316                v1cmsg=err(5);
040000160316                leavesr;
040100160316          endif;
040200160316          if v1caas=0;
040300160316                *in28=*on;
040400160316                *in90=*on;
040500160316                *in55=*on;
040600160316                v1cmsg=err(7);
040700160316                leavesr;
040800160316          endif;
040900160316
041000160330          wsel3=1  ;
041100160316          chain (v1caas:v1clp2:v1cnrs:v1cnsp) fnarb01l;
041200160316          exsr ctrbolla;
041300160316          if errbolla='1';
041400160316             *in45=*on;
041500160316             leavesr;
041600160316          endif;
041700160316       endif;
041800160316       //?Effettuare una scelta
041900160316       if wsel1=0 and wsel2=0 and wsel3=0;
042000160316             *in28=*on;
042100160316             *in90=*on;
042200160316             *in42=*on;
042300160316             v1cmsg=err(3);
042400160316             leavesr;
042500160316       endif;
042600160316       //?Se più di una scelta errore
042700160316       if wsel1+wsel2+wsel3>1           ;
042800160316             *in28=*on;
042900160316             *in90=*on;
043000160316             v1cmsg=err(13);
043100160316             leavesr;
043200160316       endif ;
043300160316
043400160316       // I prossimi controlli li salto in quanto non visualizzo per
043500160316       // il momento a video questi campi
043600160316       leavesr;
043700160316
043800160316       //?Data spedizione dal/al + lna
043900160316       // Data dal
044000160316          clear comdtd ;
044100160316          clear comdta ;
044200160316       if v1cdtd>0;
044300160316       if %subst(%editc(v1cdtd:'X'):1:2)='00';
044400160316          test(DE) *dmy v1cdtd;
044500160316          if %error;
044600160316             *in90=*on;
044700160316             *in51=*on;
044800160316             *in28=*on;
044900160316             v1cmsg=err(8);
045000160316             leavesr;
045100160316          else;
045200160316             dataiso=%date(v1cdtd:*dmy);
045300160316          endif;
045400160316       else;
045500160317          test(DE) *eur v1cdtd;
045600160316          if %error;
045700160316             *in90=*on;
045800160316             *in52=*on;
045900160316             *in28=*on;
046000160316             v1cmsg=err(8);
046100160316             leavesr;
046200160316          else;
046300160316             dataiso=%date(v1cdtd:*eur);
046400160316          endif;
046500160316       endif;
046600160316          dataeur=dataiso;
046700160316          v1cdtd=%dec(dataeur);
046800160316          comdtd=%dec(dataiso);
046900160316       endif;
047000160316       // Data al
047100160316       if v1cdta>0;
047200160316       if %subst(%editc(v1cdta:'X'):1:2)='00';
047300160316          test(DE) *dmy v1cdta;
047400160316          if %error;
047500160316             *in90=*on;
047600160316             *in52=*on;
047700160316             *in28=*on;
047800160316             v1cmsg=err(8);
047900160316             leavesr;
048000160316          else;
048100160316             dataiso=%date(v1cdta:*dmy);
048200160316          endif;
048300160316       else;
048400160317          test(DE) *eur v1cdta;
048500160316          if %error;
048600160316             *in90=*on;
048700160316             *in52=*on;
048800160316             *in28=*on;
048900160316             v1cmsg=err(8);
049000160316             leavesr;
049100160316          else;
049200160316             dataiso=%date(v1cdta:*eur);
049300160316          endif;
049400160316       endif;
049500160316          dataeur=dataiso;
049600160316          v1cdta=%dec(dataeur);
049700160316          comdta=%dec(dataiso);
049800160316       endif;
049900160316       // Se non immessa data al imposto come data dal
050000160316       if comdta=0;
050100160316          comdta=comdtd;
050200160316       endif ;
050300160316       // errore se data al minore di data dal
050400160316       if comdta<comdtd;
050500160316             *in90=*on;
050600160316             *in52=*on;
050700160316             *in28=*on;
050800160316             v1cmsg=err(10);
050900160316             leavesr;
051000160316       endif;
051100160316       //?Linea di arrivo
051200160316       clear v1cdea;
051300160316       if v1clna>0;
051400160316          chain v1clna azorg01l;
051500160316          if not %found(azorg01l)  or (orgfag<>'F' and orgfag<>'A') or
051600160316                                       orgfva<>*blanks      ;
051700160316             *in28=*on;
051800160316             *in46=*on;
051900160316             v1cmsg=err(2);
052000160316             leavesr;
052100160316          endif;
052200160316          v1cdea=orgdes;
052300160316       endif;
052400160316       if comdtd>0 and v1clna=0;
052500160316             *in28=*on;
052600160316             *in46=*on;
052700160316             v1cmsg=err(2);
052800160316             leavesr;
052900160316       endif ;
053000160316       endsr;
053100160316       //--------------------------------------------------------------
053200160316       //?Verfica che la bolla richiesta abbia i requisiti di bolla "VEDI PACCO"
053300160316       //--------------------------------------------------------------
053400160316       BEGSR ctrbolla       ;
053500160316       errbolla=' ';
053600160316       if not %found(fnarb01l);
053700160316             errbolla='1';
053800160317             *in90=*on;
053900160317             *in28=*on;
054000160316             v1cmsg=err(14);
054100160316             leavesr ;
054200160316       endif;
054300160316       // Deve essere Import DPD
054400160316       if wsel3=0;
054500160316          clear og143;
054600160316          chain arblnp azorg01l;
054700160316          if %found(azorg01l);
054800160316             og143=orgde3;
054900160316          endif;
055000160316          if §ogntw<>'DPD' ;
055100160316             errbolla='1';
055200160317             *in90=*on;
055300160317             *in28=*on;
055400160316             v1cmsg=err(4);
055500160316             leavesr ;
055600160316          endif;
055700160316       endif;
055800160316       // Deve avere §b4trd='1'
055900170926       // chain (arbaas:arblnp:arbnrs:arbnsp:'I') fiar401l;
056000170926
056100170926       clear dpndflo  ;
056200170926       chain (arbaas:arblnp:arbnrs:arbnsp) fipnd01l;
056300170926
056400170926       // if %found (fiar401l);
056500170926       //   dsbl4i=ar4not;
056600170926       //endif;
056700170926
056800170926         if %found(fipnd01l)  ;
056900170926            dpndflo=pndflo ;
057000170926         endif  ;
057100170926
057200170926       //if not %found(fiar401l) or §b4trd<>'1';
057300170926
057400170926         if not %found(fipnd01l) or §PNDTRD<>'1';
057500160316             errbolla='1';
057600160317             *in90=*on;
057700160317             *in28=*on;
057800170926
057900170926             //if %found(fiar401l) and §b4trd='2';
058000170926
058100170926             if %found(fipnd01l) and §PNDTRD='2';
058200160511                 v1cmsg=err(32);
058300160511             else;
058400160511                 v1cmsg=err(4);
058500160511             endif;
058600160317             leavesr ;
058700160316       endif;
058800170926
058900160317       // Deve essere una bolla manutenzionabile dall'utente
059000160321       //if %lookup(%editc(arblna:'X'):$pog)=0;
059100160321         if %lookup(arblna:lint)=0;
059200160317             errbolla='1';
059300160317             *in90=*on;
059400160317             *in28=*on;
059500160321       //    v1cmsg=err(19);
059600160321             v1cmsg=err(20);
059700160317             leavesr ;
059800160317       endif;
059900160316       endsr;
060000160317       //-------------------------------------------------------------*
060100160317       //Richiamo pgm di caricamento variazioni bolle FNLR48R         *
060200160317       //-------------------------------------------------------------*
060300160317        Begsr sr_lr48      ;
060400160317
060500160317        clear fnlr48ds;
060600160317        clear dsarbg;
060700160317        clear dsarbd;
060800160317        clear dsarbk;
060900160317        clear darbt;
061000160317        d48aas=arbaas;
061100160317        d48lnp=arblnp;
061200160317        d48nrs=arbnrs;
061300160317        d48nsp=arbnsp;
061400160317       // Come p.o. gestione prendo il p.o gestore (da £6) della linea di arrivo
061500160317       // p.o. di consegna capofila della £6 del p.o. di arrivo
061600160317        clear  fnlv55ds;
061700160317        d55tpt = '6' ;
061800160317        d55lin=arblna;
061900160317        d55drf=%dec(%date());
062000160317        fnlv55r(fnlv55ds);
062100160317        d48fgs=d55tfa;
062200160317        d48cbo=arbcbo;
062300160317        d48tbo='A'   ;
062400160317        d48cvb='I0';
062500160317           d48ffr='V'   ;
062600160330           d48f12='S'   ;
062700160317
062800160317        kpjbu=fnlr48ds;
062900160317        fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
063000160331        fnlr48ds=kpjbu;
063100160317
063200160317        endsr;
063300160317      /end-free
063400940321      **************************************************************************
063500940713      *     R O U T I N E      I N I Z I A L E
063600940713      **************************************************************************
063700940713     C     *INZSR        BEGSR
063800060130     c
063900060130      *
064000060130     c     *dtaara       define    §azute        azuteds
064100060130     c     *dtaara       define    §datiute      ddatiute
064200060130     c                   in(E)     *dtaara
064300060130    1c                   If        %error  or RSUT = *blanks
064400060130     c                   Clear                   Tibs34ds
064500060130     c                   Call      'TIBS34R'
064600060130     c                   Parm                    Tibs34ds
064700060130     c                   In        *dtaara
064800060130    1c                   EndIf
064900060130
065000060130     c                   Clear                   wabi
065100060130     c                   Clear                   v1cmsg
065200060130     c                   Clear                   dLat
065300060130
065400060130      * Verifica errori e autorità profilo
065500060130s   1c                   Select
065600060130      * se ho errori nei dati utente esco dal pgm
065700060130w   1c                   When      DutErr = 'E'
065800160317     c                   eval      *inlr=*on
065900160317     c                   return
066000060130      * se non c'è l'abilitazione
066100060130      * --> se 1° livello, abilitazioni al terminal
066200060130      *     se 2° livello, abilitazioni al punto operativo
066300060130      *     se sede è impossibile ma se capita mando a fine il pgm
066400060130w   1c                   When      UteAut = *Blanks
066500060130if  2c                   If        DutLpo = '1'
066600060130     c                   Eval      wabi   = 'TP'
066700060130e   2c                   EndIf
066800060130if  2c                   If        DutLpo = '2'
066900060130     c                   Eval      wabi   = 'PO'
067000060130e   2c                   EndIf
067100060130if  2c                   If        DutLpo = 'S'
067200160317     c                   eval      *inlr=*on
067300160317     c                   return
067400060130e   2c                   EndIf
067500060130      * carica le abilitazioni del profilo
067600060130x   1c                   Other
067700060130     c                   Movel     UteFaf        Dute01
067800060131     c                   If        §Utembf <> *Blanks
067900060131     c                   Eval      wabi = §Utembf
068000060130     c                   Else
068100060130     c                   Eval      wabi = UteAut
068200060130     c                   EndIf
068300060130e   1c                   EndSl
068400060130
068500060130      * controllo se ok l'abilitazione dell'utente
068600060130     c                   Clear                   Tibs02ds
068700060130     c                   Eval      T02Mod = 'C'
068800060130     c                   Eval      T02Sif = knsif
068900060130     c                   Eval      T02Cod = 'LAT'
069000060130     c                   Movel(p)  wabi          T02Ke1
069100060130     c                   Call      'TIBS02R'
069200060130     c                   Parm                    kpjba
069300060130     c                   Parm                    Tibs02ds
069400060130if  1c                   If        T02Err = *Blanks
069500060130     c                   Eval      dLat = T02Uni
069600060130e   1c                   EndIf
069700060130      * errore o non abilitato
069800060130if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
069900060130     c                   Seton                                        28
070000160317     C                   movel     err(26)       v1cmsg
070100060130   x1c                   Else
070200060130      * Reperimento dei P.O. gestibili dall'utente
070300060130     c                   clear                   TRUL31DS
070400060130     c                   clear                   TRUL31DS2
070500060130     c                   eval      I31abi = wabi
070600060130     c                   eval      I31cdi = DUTdis
070700060130     c                   eval      I31car = DUTare
070800060130     c                   eval      I31cpo = DUTpou
070900060130     c                   call      'TRUL31R'
071000060130     c                   parm                    KPJBA
071100060130     c                   parm                    TRUL31DS
071200060130     c                   parm                    TRUL31DS2
071300060130if  2c                   if        O31pog > *zeros
071400060130     c                   movea     O31pog        $pog
071500060130x   2c                   else
071600060130     c                   Seton                                        28
071700160317     c                   movel     err(26)       v1cmsg
071800060130e   2c                   endif
071900060130    1c                   endif
072000060130     c
072100160321     C* Aggancio la £6 e recupero le linee a loro associate
072200160321     c                   clear                   trul06ds
072300160321     c                   eval      d06cod = '£6'
072400160321     c                   movel     dutpou        d06key
072500160321     c                   movel(p)  trul06ds      kpjbu
072600160321     c                   call      'TRUL06R'
072700160321     c                   parm                    kpjba
072800160321     c                   movel(p)  kpjbu         trul06ds
072900131007      /free
073000131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
073100131007      /end-free
073200131007
073300940713      *
073400940713     C                   ENDSR
073500160317      /free
073600160115     C**
073700131111
073800940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
073900160316Immettere il Barcode OPPURE digitare i dati del segnacollo                     1
074000940713Immettere linea di arrivo valida                                               2
074100160316Effettuare una scelta                                                          3
074200160316I dati immessi non corrispondono ad una spedizione "VEDI PACCO" DPD            4
074300160316Immettere una linea di partenza DPD valida                                     5
074400940713Numero spedizione inesistente                                                  6
074500940713Immettere anno spedizione                                                      7
074600940713Immettere data formalmente valida                                              8
074700040216Immettere tipo bolla valido                                                    9
074800940713Immettere data di spedizione iniziale minore o uguale a quella finale         10
074900160316Non trovata spedizione per il Numero Parcel richiesto                         11
075000160316Per il n.ro Parcel richiesto sono presenti più spedizioni                     12
075100160316Effettuare UNA SOLA scelta                                                    13
075200160323Non trovata spedizione                                                        14
075300160331ATTENZIONE: la Sped. non è stata tolta da distinta per problemi di allocazione15
075400160316                                                                              16
075500940923Immettere filiale segnacollo valida                                           17
075600160316
075700941212Bolla non manutenzionabile: in gestione ad altra filiale                      19
075800160321Linea arrivo spedizione non in gestione                                       20
075900160316                                                                              21
076000160316                                                                              22
076100160316                                                                              23
076200160316                                                                              24
076300160316                                                                              25
076400060130Utente non abilitato alla Variazione bolle                                    26
076500060130Utente non abilitato alla variazione bolle del p.o. in gestione               27
076600160316                                                                              28
076700160316                                                                              29
076800160316                                                                              30
076900160316                                                                              31
077000160511"VEDI PACCO" già manutenzionato: utilizzare la Manutenzione bolle Arrivi      32
077100160316Numero Parcel DPD non valido                                                  33
077200160511Barcode errato o collo di spedizione non in arrivo                            34
077300160316Filiale mancante o errata                                                     35
