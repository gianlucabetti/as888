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
001500160316     FFiAR401L  IF   E           K DISK
001600160316     FFiAR403L  IF   E           K DISK
001700160317     f                                     rename(fiar4000:fiar403)
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
006000060518     D DSBL4I        E DS
006100941014      *
006200941212     D* DS PER TRUL06R - CARICAMENTO £X
006300991027     D TRUL06DS      E DS
006400160321     D  lint                   1     90  0 DIM(30)
006500941212      *
006600941014     D* DS PER FNLR48R - MANUTENZIONE BOLLE ARRIVI
006700991027     D FNLR48DS      E DS
006800160317     d fnlv55ds      e ds                  inz
006900160317     d trul90ds      e ds                  inz
007000160317     d dsarbd        e ds                  inz
007100160317     d dsarbk        e ds                  inz
007200160317     d darbt         e ds                  inz
007300160317     d dsarbg        e ds                  inz
007400160317     d dsarbn        e ds                  inz
007500941014      *
007600160317     d Tibs02Ds      e ds
007700060130     d Azuteds       e ds                  extname(Azute00f)
007800060130     d dDatiute      e ds
007900060130     d dLat          e ds
008000060130     d dute01        e ds
008100060130     D TRUL31DS      E DS
008200060130     D TRUL31DS2     E DS
008300060130     D TIBS34DS      E DS
008400160317     D og143         E DS
008500060130     d wabi            s                   like(UteAut)
008600160317     d krpe            s                   like(ar4n30)
008700160317     d comdta          s                   like(v1cdta)
008800160317     d comdtd          s                   like(v1cdtd)
008900030703
009000160317     d Dataiso         s               d   datfmt(*iso)
009100160317     d Dataeur         s               d   datfmt(*eur)
009200160316     d pos             s              2  0
009300160317     d contP           s              2  0
009400160317     d errbolla        s              1
009500160317     d wesci           s              1
009600160317     d wsel1           s              1  0
009700160317     d wsel2           s              1  0
009800160317     d wsel3           s              1  0
009900160330     d S_AR4AAS        s                   LIKE(AR4AAS)
010000160330     d S_AR4LNP        s                   LIKE(AR4LNP)
010100160330     d S_AR4NRS        s                   LIKE(AR4NRS)
010200160330     d S_AR4NSP        s                   LIKE(AR4NSP)
010300131111     d
010400991027      *
010500160316      * - Flags booleani
010600160316     d $Fine           s               n   inz(*off)
010700160316     d $InzD01         s               n   inz(*on)
010800160316     D digits          c                   '0123456789'
010900160317
011000160317       //--------------------------------------------------------------
011100160317       //?Definizione procedure.
011200160317       //--------------------------------------------------------------
011300160317
011400160317     D fnlv55r         PR
011500160317     D                                     extpgm('FNLV55R')
011600160317     D fnlv55ds                            likeds(fnlv55ds)
011700160317     D fnlr36r         PR
011800160317     D                                     extpgm('FNLR36R')
011900160317     D kpjba                               likeds(kpjba)
012000160317      // ? PROTOTIPI ?
012100160317      /copy gaitrasrc/srcprotopr,fnlr48r
012200900518     C****************************************************************
012300900518     C*  RIEPILOGO INDICATORI
012400900518     C***************************************************************
012500131111     C* 40-59 - ERRORI
012600920402     C* 90    - INDICA LA PRESENZA DI UN ERRORE
012700900518     C*****************************************************************
012800000000     C     *ENTRY        PLIST
012900000000     C                   PARM                    KPJBA
013000131210     c
013100160316     C                   Z-ADD     1             CODUT             1 0
013200160316     c                   Movel     rsut          V1cdsa
013300131210      /free
013400131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013500160316
013600160316       //?Gestione video
013700160316       DOW $Fine = *off;
013800160316             exsr GesD01;
013900160316       ENDDO;
014000160316
014100160316       eval *inlr=*on;
014200160316       //-------------------------------------------------------------*
014300160316       //                                                             *
014400160316       //-------------------------------------------------------------*
014500160316       Begsr Gesd01;
014600160316       // Inizializzazione videata
014700160316          if $inzd01;
014800160317              clear v1clp2 ;
014900160317              clear v1cnrs ;
015000160317              clear v1cnsp ;
015100160317              clear v1cnscs;
015200160317              clear v1cfls ;
015300160317              clear v1cdtd ;
015400160317              clear v1cdta ;
015500160317              clear v1clna ;
015600160317              clear v1cdea ;
015700160317              clear v1cnrd ;
015800160317              clear v1cbcodeb;
015900160317              clear v1clnas  ;
016000160317      /end-free
016100160317     C                   z-add     UYEAR         V1CAAS
016200160317     c     uyear         iflt      60
016300160317     c                   movel     20            v1caas
016400160317     c                   else
016500160317     C                   MOVEL     19            V1CAAS
016600160317     c                   endif
016700160317      /free
016800160316              $InzD01  = *off;
016900160316          endif;
017000160316             exfmt lri7d01;
017100160316             clear v1cmsg;
017200160316             *in28=*off;
017300160316             *in90=*off;
017400160316             select;
017500160316             // F3=Fine
017600160316                when *inkc;
017700160316                   $fine=*on;
017800160316                   leavesr;
017900160317             // F7=Int. Bolle
018000160317                when *inkg;
018100160317                 clear  kpjbu ;
018200160317                 clear  param1;
018300160317                 PA1AAS=v1caas;
018400160317                 PA1LNP=v1clp2;
018500160317                 PA1NRS=v1cnrs;
018600160317                 PA1NSP=v1cnsp;
018700160317                 if pa1nsp=0;
018800160317                    PA1FLG='2' ;
018900160317                 else;
019000160317                    PA1FLG='1' ;
019100160317                 endif;
019200160317                 clear  PA1CML;
019300160317                 PA1PGM = N_pgm  ;
019400160317                 kpjbu=param1;
019500160317                 FNLR36R(kpjba)  ;
019600160317                 param1=kpjbu;
019700160317                 if pa1nsp>0;
019800160317                    v1caas=pa1aas ;
019900160317                    v1clp2=pa1lnp ;
020000160317                    v1cnrs=pa1nrs ;
020100160317                    v1cnsp=pa1nsp;
020200160317                 endif;
020300160317                 leavesr;
020400160316                other;
020500160316                   exsr ctrd01;
020600160316                   if *in90;
020700160316                      leavesr;
020800160316                   endif;
020900160317                      exsr sr_lr48;
021000160401       //             if d48err=*blanks;
021100160331                         $inzd01=*on;
021200160401       //             else ;
021300160401       //                v1cmsg=err(15);
021400160401       //                *in28=*on;
021500160401       //             endif;
021600160318                      leavesr;
021700160316             endsl;
021800160316       endsr;
021900160316       //--------------------------------------------------------------
022000160316       //?Controlli videata
022100160316       //--------------------------------------------------------------
022200160316       BEGSR ctrd01         ;
022300160316          *in41=*off;
022400160316          *in42=*off;
022500160316          *in43=*off;
022600160316          *in44=*off;
022700160316          *in45=*off;
022800160316          *in46=*off;
022900160316          *in51=*off;
023000160316          *in52=*off;
023100160316          *in55=*off;
023200160316          wsel1=0      ;
023300160316          wsel2=0      ;
023400160316          wsel3=0      ;
023500160316       //?Barcode Bartolini letto da pistola OPPURE digitato a mano
023600160316       if v1cbcodeb>0 and (v1cfls>0 or v1clnas>0 or v1cnscs>0);
023700160316                *in28=*on;
023800160316                *in90=*on;
023900160316                *in42=*on;
024000160316                v1cmsg=err(1);
024100160316                leavesr;
024200160316       endif;
024300160316       //?Barcode Bartolini  (lettura  con pistola)
024400160316       if   V1CBCODEB>0    ;
024500160316          chain (wbcodfls:wbcodlna:wbcodnrs:wbcodnsc) fnart27l    ;
024600160316          if not %found(fnart27l);
024700160316                *in28=*on;
024800160316                *in90=*on;
024900160316                *in42=*on;
025000160316                v1cmsg=err(34);
025100160316                leavesr;
025200160316          endif;
025300160316          wsel1=1  ;
025400160316          chain (artaas:artlnp:artnrs:artnsp) fnarb01l;
025500160316          exsr ctrbolla;
025600160316          if errbolla='1';
025700160316             *in42=*on;
025800160316             leavesr;
025900160316          endif;
026000160316       endif;
026100160316       //?Segnacollo Bartolini (digitato a mano)
026200160316       if   v1cfls>0 or v1clnas>0 or v1cnscs>0;
026300160316       // Filiale segnacollo
026400160316          chain v1cfls azorg01l;
026500160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
026600160316                                      orgfva<>*blanks      ;
026700160316                *in28=*on;
026800160316                *in90=*on;
026900160316                *in43=*on;
027000160316                v1cmsg=err(35);
027100160316                leavesr;
027200160316          endif ;
027300160316       // linea di arrivo
027400160316          chain v1clnas azorg01l;
027500160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
027600160316                                      orgfva<>*blanks      ;
027700160316                *in28=*on;
027800160316                *in90=*on;
027900160316                *in44=*on;
028000160316                v1cmsg=err(35);
028100160316                leavesr;
028200160316          endif ;
028300160316          chain (v1cfls:v1clnas:v1cnrss:v1cnscs) fnart27l    ;
028400160316          if not %found(fnart27l);
028500160316                *in28=*on;
028600160316                *in90=*on;
028700160316                *in43=*on;
028800160316                v1cmsg=err(34);
028900160316                leavesr;
029000160316          endif;
029100160316          wsel1=1   ;
029200160316          chain (artaas:artlnp:artnrs:artnsp) fnarb01l;
029300160316          exsr ctrbolla;
029400160316          if errbolla='1';
029500160316             *in43=*on;
029600160316             leavesr;
029700160316          endif;
029800160316       endif;
029900160316       //?Parcel DPD
030000160316       if    v1cnrd<>*blanks;
030100160316             if %scan(' ':v1cnrd)>0 ;
030200160316                *in28=*on;
030300160316                *in90=*on;
030400160316                *in41=*on;
030500160316                v1cmsg=err(33);
030600160316                leavesr;
030700160316             endif;
030800160316       // Controllo se contiene solo numeri
030900160316       // Possono esserci lettere solo nei primi 4 byte. Dal quinto in poi devono
031000160316       // essere solo numeri
031100160316             pos=%check(digits:v1cnrd);
031200160316             if pos>4;
031300160316                *in28=*on;
031400160316                *in90=*on;
031500160316                *in41=*on;
031600160316                v1cmsg=err(33);
031700160316                leavesr;
031800160316             endif;
031900160316       // Cerco la spedizione
032000160316          krpe=v1cnrd;
032100160316          contp=0;
032200160330          CLEAR s_ar4aas       ;
032300160330          CLEAR s_ar4LNP       ;
032400160330          CLEAR s_ar4NRS       ;
032500160330          CLEAR s_ar4NSP       ;
032600160316          wesci=*blanks;
032700160316          setll ('I':krpe) fiar403l  ;
032800160316          reade 'I' fiar403l;
032900160316          dow not %eof(fiar403l) and wesci=*blanks ;
033000160316             dsbl4i=ar4n30;
033100160316             if  §b4ipn= v1cnrd;
033200160330                s_ar4aas=ar4AAS;
033300160330                s_ar4LNP=ar4LNP;
033400160330                s_ar4NRS=ar4NRS;
033500160330                s_ar4NSP=ar4NSP;
033600160316                contP+=1;
033700160316             else;
033800160316                wesci='1';
033900160316             endif;
034000160330             if wesci = *blanks;
034100160330                reade 'I' fiar403l;
034200160330             endif;
034300160316          enddo;
034400160316       // Errore se presente più di una spedizione con lo stesso numero parcel
034500160316          if contP>1;
034600160316                *in28=*on;
034700160316                *in90=*on;
034800160316                *in41=*on;
034900160316                v1cmsg=err(12);
035000160316             leavesr;
035100160316          endif;
035200160316       // Errore se non trovata neanche una spedizione
035300160316          if contP=0;
035400160316                *in28=*on;
035500160316                *in90=*on;
035600160316                *in41=*on;
035700160316                v1cmsg=err(11);
035800160316             leavesr;
035900160316          endif;
036000160316
036100160330          wsel2=1  ;
036200160330          chain (S_Ar4aas:S_ar4lnp:S_ar4nrs:S_ar4nsp) fnarb01l;
036300160316          exsr ctrbolla;
036400160316          if errbolla='1';
036500160316             *in41=*on;
036600160316             leavesr;
036700160316          endif;
036800160316       endif;
036900160316       //?Key spedizione
037000160316       if   v1clp2>0 or v1cnsp>0;
037100160316          chain v1clp2 azorg01l;
037200160316          if not %found(azorg01l) or (orgfag<>'F' and orgfag<>'A') or
037300160316                                      orgfva<>*blanks      ;
037400160316                *in28=*on;
037500160316                *in90=*on;
037600160316                *in45=*on;
037700160316                v1cmsg=err(5);
037800160316                leavesr;
037900160316          endif ;
038000160316          og143=orgde3;
038100160316          if §ogntw<>'DPD';
038200160316                *in28=*on;
038300160316                *in90=*on;
038400160316                *in45=*on;
038500160316                v1cmsg=err(5);
038600160316                leavesr;
038700160316          endif;
038800160316          if v1caas=0;
038900160316                *in28=*on;
039000160316                *in90=*on;
039100160316                *in55=*on;
039200160316                v1cmsg=err(7);
039300160316                leavesr;
039400160316          endif;
039500160316
039600160330          wsel3=1  ;
039700160316          chain (v1caas:v1clp2:v1cnrs:v1cnsp) fnarb01l;
039800160316          exsr ctrbolla;
039900160316          if errbolla='1';
040000160316             *in45=*on;
040100160316             leavesr;
040200160316          endif;
040300160316       endif;
040400160316       //?Effettuare una scelta
040500160316       if wsel1=0 and wsel2=0 and wsel3=0;
040600160316             *in28=*on;
040700160316             *in90=*on;
040800160316             *in42=*on;
040900160316             v1cmsg=err(3);
041000160316             leavesr;
041100160316       endif;
041200160316       //?Se più di una scelta errore
041300160316       if wsel1+wsel2+wsel3>1           ;
041400160316             *in28=*on;
041500160316             *in90=*on;
041600160316             v1cmsg=err(13);
041700160316             leavesr;
041800160316       endif ;
041900160316
042000160316       // I prossimi controlli li salto in quanto non visualizzo per
042100160316       // il momento a video questi campi
042200160316       leavesr;
042300160316
042400160316       //?Data spedizione dal/al + lna
042500160316       // Data dal
042600160316          clear comdtd ;
042700160316          clear comdta ;
042800160316       if v1cdtd>0;
042900160316       if %subst(%editc(v1cdtd:'X'):1:2)='00';
043000160316          test(DE) *dmy v1cdtd;
043100160316          if %error;
043200160316             *in90=*on;
043300160316             *in51=*on;
043400160316             *in28=*on;
043500160316             v1cmsg=err(8);
043600160316             leavesr;
043700160316          else;
043800160316             dataiso=%date(v1cdtd:*dmy);
043900160316          endif;
044000160316       else;
044100160317          test(DE) *eur v1cdtd;
044200160316          if %error;
044300160316             *in90=*on;
044400160316             *in52=*on;
044500160316             *in28=*on;
044600160316             v1cmsg=err(8);
044700160316             leavesr;
044800160316          else;
044900160316             dataiso=%date(v1cdtd:*eur);
045000160316          endif;
045100160316       endif;
045200160316          dataeur=dataiso;
045300160316          v1cdtd=%dec(dataeur);
045400160316          comdtd=%dec(dataiso);
045500160316       endif;
045600160316       // Data al
045700160316       if v1cdta>0;
045800160316       if %subst(%editc(v1cdta:'X'):1:2)='00';
045900160316          test(DE) *dmy v1cdta;
046000160316          if %error;
046100160316             *in90=*on;
046200160316             *in52=*on;
046300160316             *in28=*on;
046400160316             v1cmsg=err(8);
046500160316             leavesr;
046600160316          else;
046700160316             dataiso=%date(v1cdta:*dmy);
046800160316          endif;
046900160316       else;
047000160317          test(DE) *eur v1cdta;
047100160316          if %error;
047200160316             *in90=*on;
047300160316             *in52=*on;
047400160316             *in28=*on;
047500160316             v1cmsg=err(8);
047600160316             leavesr;
047700160316          else;
047800160316             dataiso=%date(v1cdta:*eur);
047900160316          endif;
048000160316       endif;
048100160316          dataeur=dataiso;
048200160316          v1cdta=%dec(dataeur);
048300160316          comdta=%dec(dataiso);
048400160316       endif;
048500160316       // Se non immessa data al imposto come data dal
048600160316       if comdta=0;
048700160316          comdta=comdtd;
048800160316       endif ;
048900160316       // errore se data al minore di data dal
049000160316       if comdta<comdtd;
049100160316             *in90=*on;
049200160316             *in52=*on;
049300160316             *in28=*on;
049400160316             v1cmsg=err(10);
049500160316             leavesr;
049600160316       endif;
049700160316       //?Linea di arrivo
049800160316       clear v1cdea;
049900160316       if v1clna>0;
050000160316          chain v1clna azorg01l;
050100160316          if not %found(azorg01l)  or (orgfag<>'F' and orgfag<>'A') or
050200160316                                       orgfva<>*blanks      ;
050300160316             *in28=*on;
050400160316             *in46=*on;
050500160316             v1cmsg=err(2);
050600160316             leavesr;
050700160316          endif;
050800160316          v1cdea=orgdes;
050900160316       endif;
051000160316       if comdtd>0 and v1clna=0;
051100160316             *in28=*on;
051200160316             *in46=*on;
051300160316             v1cmsg=err(2);
051400160316             leavesr;
051500160316       endif ;
051600160316       endsr;
051700160316       //--------------------------------------------------------------
051800160316       //?Verfica che la bolla richiesta abbia i requisiti di bolla "VEDI PACCO"
051900160316       //--------------------------------------------------------------
052000160316       BEGSR ctrbolla       ;
052100160316       errbolla=' ';
052200160316       if not %found(fnarb01l);
052300160316             errbolla='1';
052400160317             *in90=*on;
052500160317             *in28=*on;
052600160316             v1cmsg=err(14);
052700160316             leavesr ;
052800160316       endif;
052900160316       // Deve essere Import DPD
053000160316       if wsel3=0;
053100160316          clear og143;
053200160316          chain arblnp azorg01l;
053300160316          if %found(azorg01l);
053400160316             og143=orgde3;
053500160316          endif;
053600160316          if §ogntw<>'DPD' ;
053700160316             errbolla='1';
053800160317             *in90=*on;
053900160317             *in28=*on;
054000160316             v1cmsg=err(4);
054100160316             leavesr ;
054200160316          endif;
054300160316       endif;
054400160316       // Deve avere §b4trd='1'
054500160316       chain (arbaas:arblnp:arbnrs:arbnsp:'I') fiar401l;
054600160316       if %found (fiar401l);
054700160316          dsbl4i=ar4not;
054800160316       endif;
054900160316       if not %found(fiar401l) or §b4trd<>'1';
055000160316             errbolla='1';
055100160317             *in90=*on;
055200160317             *in28=*on;
055300160316             v1cmsg=err(4);
055400160317             leavesr ;
055500160316       endif;
055600160317       // Deve essere una bolla manutenzionabile dall'utente
055700160321       //if %lookup(%editc(arblna:'X'):$pog)=0;
055800160321         if %lookup(arblna:lint)=0;
055900160317             errbolla='1';
056000160317             *in90=*on;
056100160317             *in28=*on;
056200160321       //    v1cmsg=err(19);
056300160321             v1cmsg=err(20);
056400160317             leavesr ;
056500160317       endif;
056600160316       endsr;
056700160317       //-------------------------------------------------------------*
056800160317       //Richiamo pgm di caricamento variazioni bolle FNLR48R         *
056900160317       //-------------------------------------------------------------*
057000160317        Begsr sr_lr48      ;
057100160317
057200160317        clear fnlr48ds;
057300160317        clear dsarbg;
057400160317        clear dsarbd;
057500160317        clear dsarbk;
057600160317        clear darbt;
057700160317        d48aas=arbaas;
057800160317        d48lnp=arblnp;
057900160317        d48nrs=arbnrs;
058000160317        d48nsp=arbnsp;
058100160317       // Come p.o. gestione prendo il p.o gestore (da £6) della linea di arrivo
058200160317       // p.o. di consegna capofila della £6 del p.o. di arrivo
058300160317        clear  fnlv55ds;
058400160317        d55tpt = '6' ;
058500160317        d55lin=arblna;
058600160317        d55drf=%dec(%date());
058700160317        fnlv55r(fnlv55ds);
058800160317        d48fgs=d55tfa;
058900160317        d48cbo=arbcbo;
059000160317        d48tbo='A'   ;
059100160317        d48cvb='I0';
059200160317           d48ffr='V'   ;
059300160330           d48f12='S'   ;
059400160317
059500160317        kpjbu=fnlr48ds;
059600160317        fnlr48r(kpjba:dsarbd:dsarbk:darbt:dsarbg:trul90ds);
059700160331        fnlr48ds=kpjbu;
059800160317
059900160317        endsr;
060000160317      /end-free
060100940321      **************************************************************************
060200940713      *     R O U T I N E      I N I Z I A L E
060300940713      **************************************************************************
060400940713     C     *INZSR        BEGSR
060500060130     c
060600060130      *
060700060130     c     *dtaara       define    §azute        azuteds
060800060130     c     *dtaara       define    §datiute      ddatiute
060900060130     c                   in(E)     *dtaara
061000060130    1c                   If        %error  or RSUT = *blanks
061100060130     c                   Clear                   Tibs34ds
061200060130     c                   Call      'TIBS34R'
061300060130     c                   Parm                    Tibs34ds
061400060130     c                   In        *dtaara
061500060130    1c                   EndIf
061600060130
061700060130     c                   Clear                   wabi
061800060130     c                   Clear                   v1cmsg
061900060130     c                   Clear                   dLat
062000060130
062100060130      * Verifica errori e autorità profilo
062200060130s   1c                   Select
062300060130      * se ho errori nei dati utente esco dal pgm
062400060130w   1c                   When      DutErr = 'E'
062500160317     c                   eval      *inlr=*on
062600160317     c                   return
062700060130      * se non c'è l'abilitazione
062800060130      * --> se 1° livello, abilitazioni al terminal
062900060130      *     se 2° livello, abilitazioni al punto operativo
063000060130      *     se sede è impossibile ma se capita mando a fine il pgm
063100060130w   1c                   When      UteAut = *Blanks
063200060130if  2c                   If        DutLpo = '1'
063300060130     c                   Eval      wabi   = 'TP'
063400060130e   2c                   EndIf
063500060130if  2c                   If        DutLpo = '2'
063600060130     c                   Eval      wabi   = 'PO'
063700060130e   2c                   EndIf
063800060130if  2c                   If        DutLpo = 'S'
063900160317     c                   eval      *inlr=*on
064000160317     c                   return
064100060130e   2c                   EndIf
064200060130      * carica le abilitazioni del profilo
064300060130x   1c                   Other
064400060130     c                   Movel     UteFaf        Dute01
064500060131     c                   If        §Utembf <> *Blanks
064600060131     c                   Eval      wabi = §Utembf
064700060130     c                   Else
064800060130     c                   Eval      wabi = UteAut
064900060130     c                   EndIf
065000060130e   1c                   EndSl
065100060130
065200060130      * controllo se ok l'abilitazione dell'utente
065300060130     c                   Clear                   Tibs02ds
065400060130     c                   Eval      T02Mod = 'C'
065500060130     c                   Eval      T02Sif = knsif
065600060130     c                   Eval      T02Cod = 'LAT'
065700060130     c                   Movel(p)  wabi          T02Ke1
065800060130     c                   Call      'TIBS02R'
065900060130     c                   Parm                    kpjba
066000060130     c                   Parm                    Tibs02ds
066100060130if  1c                   If        T02Err = *Blanks
066200060130     c                   Eval      dLat = T02Uni
066300060130e   1c                   EndIf
066400060130      * errore o non abilitato
066500060130if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
066600060130     c                   Seton                                        28
066700160317     C                   movel     err(26)       v1cmsg
066800060130   x1c                   Else
066900060130      * Reperimento dei P.O. gestibili dall'utente
067000060130     c                   clear                   TRUL31DS
067100060130     c                   clear                   TRUL31DS2
067200060130     c                   eval      I31abi = wabi
067300060130     c                   eval      I31cdi = DUTdis
067400060130     c                   eval      I31car = DUTare
067500060130     c                   eval      I31cpo = DUTpou
067600060130     c                   call      'TRUL31R'
067700060130     c                   parm                    KPJBA
067800060130     c                   parm                    TRUL31DS
067900060130     c                   parm                    TRUL31DS2
068000060130if  2c                   if        O31pog > *zeros
068100060130     c                   movea     O31pog        $pog
068200060130x   2c                   else
068300060130     c                   Seton                                        28
068400160317     c                   movel     err(26)       v1cmsg
068500060130e   2c                   endif
068600060130    1c                   endif
068700060130     c
068800160321     C* Aggancio la £6 e recupero le linee a loro associate
068900160321     c                   clear                   trul06ds
069000160321     c                   eval      d06cod = '£6'
069100160321     c                   movel     dutpou        d06key
069200160321     c                   movel(p)  trul06ds      kpjbu
069300160321     c                   call      'TRUL06R'
069400160321     c                   parm                    kpjba
069500160321     c                   movel(p)  kpjbu         trul06ds
069600131007      /free
069700131210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
069800131007      /end-free
069900131007
070000940713      *
070100940713     C                   ENDSR
070200160317      /free
070300160115     C**
070400131111
070500940713**  MESSAGGI DI ERRORE VIDEO  **************************************************
070600160316Immettere il Barcode OPPURE digitare i dati del segnacollo                     1
070700940713Immettere linea di arrivo valida                                               2
070800160316Effettuare una scelta                                                          3
070900160316I dati immessi non corrispondono ad una spedizione "VEDI PACCO" DPD            4
071000160316Immettere una linea di partenza DPD valida                                     5
071100940713Numero spedizione inesistente                                                  6
071200940713Immettere anno spedizione                                                      7
071300940713Immettere data formalmente valida                                              8
071400040216Immettere tipo bolla valido                                                    9
071500940713Immettere data di spedizione iniziale minore o uguale a quella finale         10
071600160316Non trovata spedizione per il Numero Parcel richiesto                         11
071700160316Per il n.ro Parcel richiesto sono presenti più spedizioni                     12
071800160316Effettuare UNA SOLA scelta                                                    13
071900160323Non trovata spedizione                                                        14
072000160331ATTENZIONE: la Sped. non è stata tolta da distinta per problemi di allocazione15
072100160316                                                                              16
072200940923Immettere filiale segnacollo valida                                           17
072300160316
072400941212Bolla non manutenzionabile: in gestione ad altra filiale                      19
072500160321Linea arrivo spedizione non in gestione                                       20
072600160316                                                                              21
072700160316                                                                              22
072800160316                                                                              23
072900160316                                                                              24
073000160316                                                                              25
073100060130Utente non abilitato alla Variazione bolle                                    26
073200060130Utente non abilitato alla variazione bolle del p.o. in gestione               27
073300160316                                                                              28
073400160316                                                                              29
073500160316                                                                              30
073600160316                                                                              31
073700160316                                                                              32
073800160316Numero Parcel DPD non valido                                                  33
073900160316Collo di spedizione non in arrivo                                             34
074000160316Filiale mancante o errata                                                     35
