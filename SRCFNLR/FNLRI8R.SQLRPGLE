000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000400180123      *  Nome programma:  FNLRI8R
000500180123      *  Descrizione   :  Eliminazione Segnalazione di Incompatibile da archivi bolle
000600180123      *  Data creazione:  23 GEN 2018
000700940712      *------------------------------------------------------------------------*
000800991105     H DECEDIT('0,') DATEDIT(*DMY/)
001000940712      *
001100180123     FFNLRI8D   CF   E             WORKSTN
001200040218     FAZORG01L  IF   E           K DISK
001300940712     F*.......... B O L L E   A R R I V I
001400940712     FFNARB01L  IF   E           K DISK
001401180123     FFiart01L  IF   E           K DISK
001402180123     FFiars01L  UF   E           K DISK
001902180123     FFNAGB01L  UF A E           K DISK
002000040218      *------------------------------------------------------------------------*
002100060130     D $pog            s              3    dim(250)
002200060130     d
002300180123     D err             S             78    DIM(27) CTDATA PERRCD(1)
002400911024     D*
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
006100170926
006300941014      *
006400941212     D* DS PER TRUL06R - CARICAMENTO £X
006500991027     D TRUL06DS      E DS
006600160321     D  lint                   1     90  0 DIM(30)
006700941212      *
007800160317     d Tibs02Ds      e ds
007900060130     d Azuteds       e ds                  extname(Azute00f)
008000060130     d dDatiute      e ds
008100060130     d dLat          e ds
008200060130     d dute01        e ds
008300060130     D TRUL31DS      E DS
008400060130     D TRUL31DS2     E DS
008500060130     D TIBS34DS      E DS
008700060130     d wabi            s                   like(UteAut)
008701180123     d errbolla        s              1
010700991027      *
010800160316      * - Flags booleani
010900160316     d $Fine           s               n   inz(*off)
011000160316     d $InzD01         s               n   inz(*on)
011102180123     d
011200160317
011300160317       //--------------------------------------------------------------
011400160317       //?Definizione procedure.
011500160317       //--------------------------------------------------------------
011600160317
012000160317     D fnlr36r         PR
012100160317     D                                     extpgm('FNLR36R')
012200160317     D kpjba                               likeds(kpjba)
012300160317      // ? PROTOTIPI ?
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
017300180123             exfmt lri8d01;
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
021101180123                   if *inkf;
021102180123                      exsr conferma;
021103180123                      $inzd01=*on;
021104180123                      leavesr;
021105180123                   endif;
022000160316             endsl;
022100160316       endsr;
022200160316       //--------------------------------------------------------------
022300160316       //?Controlli videata
022400160316       //--------------------------------------------------------------
022500160316       BEGSR ctrd01         ;
022900160316          *in44=*off;
023000160316          *in45=*off;
023100180123          *in49=*off;
023400160316          *in55=*off;
038200170926
038206180123          if v1caas<=99;
038207180123             if v1caas < 60;
038208180123     c                   movel     20            v1caas
038209180123             else;
038210180123     C                   MOVEL     19            V1CAAS
038211180123             endif;
038212180123          endif;
038300180123       //?Controllo key spedizione
040900160316
041100160316          chain (v1caas:v1clp2:v1cnrs:v1cnsp) fnarb01l;
041200160316          exsr ctrbolla;
041300160316          if errbolla='1';
041400160316             *in45=*on;
041500160316             leavesr;
041600160316          endif;
041601180123       //?Segnacollo obbligatorio
041602180123          if v1cnscs = 0 ;
041603180123             *in49=*on;
041604180123             *in90=*on;
041605180123             *in28=*on;
041606180123             v1cmsg=err(1);
041607180123             leavesr;
041608180123          endif;
041609180123       //?Il segnacollo richiesto deve esistere nella spedizione
041610180123          chain (v1caas:v1clp2:v1cnrs:v1cnsp:v1cnscs) fiart01l;
041611180123          if not %found(fiart01l);
041612180123             *in49=*on;
041613180123             *in90=*on;
041614180123             *in28=*on;
041615180123             v1cmsg=err(1);
041616180123             leavesr;
041617180123          endif;
043300160316
053000160316       endsr;
053100160316       //--------------------------------------------------------------
053200180123       //?Verfica esistenza della bolla richiesta
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
058800170926
058900160317       // Deve essere una bolla manutenzionabile dall'utente
059100180201         if %lookup(%editc(arblna:'X'):$pog)=0;
059200160317             errbolla='1';
059300160317             *in90=*on;
059400160317             *in28=*on;
059600160321             v1cmsg=err(20);
059700160317             leavesr ;
059800160317       endif;
059900160316       endsr;
060000160317       //-------------------------------------------------------------*
060100180123       //Eliminazione di segnalazione incompatibile                   *
060200160317       //-------------------------------------------------------------*
060300180123        Begsr Conferma     ;
060301180123          chain (v1caas:v1clp2:v1cnrs:v1cnsp:v1cnscs) fiart01l;
060302180123          if %found (fiart01l);
060303180123             chain (artfls:artlna:artnrs:artnsc:'J') fiars01l;
060304180123             if %found(fiars01l);
060305180123                delete fiars000    ;
060306180123                clear fnagb000;
060307180123                AGBTBO='A'     ;
060308180123                AGBAAS=artaas;
060309180123                AGBLNP=artlnp;
060310180123                AGBNRS=artnrs;
060311180123                AGBNSP=artnsp;
060312180123                AGBAGB='JD'  ;
060313180123                write(e) fnagb000;
060317180123             endif;
060318180123          endif;
060400160317
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
073900180123Segnacollo mancante o inesistente per la spedizione richiesta                  1
074000180123                                                                               2
074100180123                                                                               3
074200180123                                                                               4
074300180123                                                                               5
074400940713Numero spedizione inesistente                                                  6
074500180123                                                                               7
074600180123                                                                               8
074700180123                                                                               9
074800180123                                                                              10
074900180123                                                                              11
075000180123                                                                              12
075100180123                                                                              13
075200160323Non trovata spedizione                                                        14
075300180123                                                                              15
075400160316                                                                              16
075500180123                                                                              17
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
