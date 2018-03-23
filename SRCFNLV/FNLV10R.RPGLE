000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200111215     H* FNLV10R  *---------------------------------------------------*
000300111215     H*     CONTROLLO instradamenti richiamando il pgm FNLV13R / TISI95R
000400921021     H*--------------------------------------------------------------*
000500111219     Fwfeiv00f  o  a e             disk
000600111216     F
000700111215     D ERR             S             78    DIM(11) CTDATA PERRCD(1)
000800111216     D erc             s              2    dim(11) alt(err)
000900111216     D
001000111215     D*
001100111215     D I_tisi95ds    E DS                  prefix (I_)
001200111215     d                                     extname(tisi95ds)
001300111215     D dssi95        E DS                  extname(tisi95ds)
001400111215     D fnlv13ds      E DS
001500111215     D fnlv10ds      E DS
001600970707     D KPJBA         E DS
001700111219     d tibs10ds      e ds
001800111219     d TIBS69DS      E DS                  INZ
001900111219     d DS_cnaco      E DS                  extname(CNACO00F)
002000111219     d DS_cnind      E DS                  extname(CNIND00F)
002100111219     d DS_cnclp      E DS                  extname(CNCLP00F)
002200111219     d DS_fncls      E DS                  extname(FNCLS00F)
002300111219     d Tibs34ds      e ds
002400111219     d Azuteds       e ds                  Extname(Azute00f)
002500111219     d dDatiute      e ds
002600111215     d
002700111215     d WtipoLa         s              1
002800111215     d w_o95lia        s                   Like(o95lia)
002900111215     d n               s              2  0
003000921021     C*---------------------------------------------------------------*
003100921021     C     *ENTRY        PLIST
003200970707     C                   PARM                    KPJBA
003300111215     C                   PARM                    I_tisi95ds
003400111215     C                   PARM                    fnlv10ds
003500940218     C*---------------------------------------------------------------*
003600111215     C* TIPO LANCIO,    TLA "C" -           CHIUSO CON LR
003700111215     C* TIPO LANCIO,    TLA "L" - ELABORO E CHIUDO CON LR
003800111215     C* TIPO LANCIO,    TLA " " - ELABORO E CHIUDO IN RETRN
003900111215     c
004000111215     c
004100111219    0c                   if        ilv10tla<>'C'
004200111219     C                   CLEAR                   olv10narp
004300111219     C                   CLEAR                   olv10capp
004400111219     C                   CLEAR                   olv10locp
004500111219     C                   CLEAR                   olv10prvp
004600111219     C                   CLEAR                   olv10liv
004700111219     C                   CLEAR                   olv10cod
004800111219     C                   CLEAR                   olv10err
004900111220     C                   CLEAR                   w_o95lia
005000111215     c
005100111215      *     POSSONO ESSERE EFFETTUATI più TENTATIVI DI CONTROLLO:
005200111215      *     IL PRIMO: NORMALE
005300111215      *     IL SECONDO1: SE HO TROVATO CAP OBSOLETO AL PRIMO
005400111215      *     IL SECONDO2: SE ERRORE DI PROVINCIA AL PRIMO
005500111215      **
005600111215     C                   MOVEL(P)  'PRIMO'       TENTATIVO         8
005700111215     c                   clear                   obslna            3 0
005800111215     c                   clear                   obslocnor        35
005900111215      *
006000111215      * PULISCO DS CHE SERVONO A "FNLV13R"
006100111215      *
006200111215     C     RITENT        TAG
006300111215      *
006400111215     C                   CLEAR                   fnlv13ds
006500111215     C                   CLEAR                   DSSI95
006600111215      *
006700111215      * PASSO PARAMETRI A "FNLV13R"
006800111215      *
006900111215     C                   MOVEL     '7'           I95TCN
007000111215     C                   MOVEL     'S'           I95FRE
007100111215     C                   MOVEL     'S'           I13AF0
007200111215     C                   MOVEL     'S'           I13CNV
007300111215     C                   MOVEL     'S'           I13LA3
007400111215     c                   movel     'S'           flgbac
007500111215     c
007600111215      *
007700111215     C                   MOVEL     i_i95loc      I95LOC
007800111215     C     TENTATIVO     IFEQ      'PRIMO'
007900111215     C                   MOVEL     i_i95cap      I95CAP
008000111215     C                   ELSE
008100111215     C     TENTATIVO     IFEQ      'SECONDO1'
008200111215     C                   CLEAR                   I95CAP
008300111215     C                   ELSE
008400111215     C                   MOVEL     i_i95cap      I95CAP
008500111215     C                   MOVEL     '4'           I95TCN
008600111215     C                   ENDIF
008700111215     C                   ENDIF
008800111215     c
008900111215     C                   MOVEL     I_i95prv      I95PRV
009000111215     C                   MOVEL     I_i95nar      I95NAR
009100111215     c* indirizzo per ora non usato
009200111215     C                   MOVEL     I_i95ind      I95IND
009300111215     c
009400111215     c* Imposto tutti gli altri campo di input eventualmente passati
009500111215     c
009600111215     c* Se data di riferimento non passata, il tisi95 imposta UDATE
009700111215     c                   eval      i95dat=I_i95dat
009800111215     c                   eval      i95lkg=I_i95lkg
009900111215     c                   eval      i95lmc=I_i95lmc
010000111215     c                   eval      i95ffd=I_i95ffd
010100111215     c                   eval      i95tpo=I_i95tpo
010200111215     c                   eval      i95fca=I_i95fca
010300111215     c                   eval      i95tsp=I_i95tsp
010400111215     c                   eval      i95tfp=I_i95tfp
010500111215     c                   eval      i95fi1=I_i95fi1
010600111215     c                   eval      i95fi2=I_i95fi2
010700111215     c                   eval      i95fi3=I_i95fi3
010800111215      *
010900111215     c
011000111215     c* Per secondo tentativo per sicurezza usola località noramlizzata
011100111215    1C     TENTATIVO     IFEQ      'SECONDO1'
011200111215     c                   movel     obslocnor     i95loc
011300111215    1c                   endif
011400111215      *
011500111215      * LANCIO "FNLV13R"
011600111215      *
011700111215     C                   CALL      'FNLV13R'
011800111215     C                   PARM                    KPJBA
011900111215     C                   PARM                    fnlv13ds
012000111215     C                   PARM                    DSSI95
012100111215     C                   PARM                    flgbac            1
012200111215     c
012300111215     c* Se mi è stato dato errore per secondo tentativo ma
012400111215     c*  trova i dati univoci per località, se corrisponde la lna
012500111215     c*  prima e dopo, tratto come se ma deco correggere la provincia
012600111215    1c                   if        TENTATIVO='SECONDO1 '
012700111215     C                             AND O95LNA=obslna
012800111215     c                             and %subst(O95MSG:78:1) = '9'
012900111215     c                             and o95err=' '
013000111215     c                   eval      o13rop='S'
013100111215     c                   eval      o13roc='S'
013200111215    1c                   endif
013300111215      *
013400111215      *
013500111215    1C     O95ERR        IFNE      *BLANKS
013600111215     C     O95ERR        andne     'E'
013700111215     c
013800111215     C     TENTATIVO     ORNE      'PRIMO'
013900111215     c
014000111215     C                   EXSR      w_file
014100111215     c
014200111215   X1C                   ELSE
014300111215      *
014400111215     c
014500111215      * SE NON VI E' ERRORE MA CAP = OBSOLETO RITENTO CLEARANDO IL CAP
014600111215      *
014700111215    2c                   if        o95err=*blanks and
014800111215     c                             tentativo='PRIMO ' and
014900111215     c                             (o95cf1<>*blanks or o95lf1<>*blanks)
015000111215      *
015100111215     C                   MOVEL     'SECONDO1'    TENTATIVO
015200111215     c* Mi salvo la linea di arrivo del cap obsoleto
015300111215     c                   eval      obslna=o95lna
015400111215     c* Mi salvo anche la località normalizzata per la seconda ricerca
015500111215     c                   eval      obslocnor=o95loc
015600111215     c                   eval      w_o95lia='O'
015700111215      *
015800111215     C                   GOTO      RITENT
015900111215    2C                   ENDIF
016000111215      *
016100111215      * SE VI E' ERRORE DI PROVINCIA = RITENTO CON TIPO CONTROLLO 4
016200111215      * E LIVELLO DATI >= 3 E LIVELLO AFFIDABILITA >= 2
016300111215      *
016400111215    2C                   IF        (%subst(O95MSG:78:1) = '9'  OR
016500111215     C                              %subst(O95MSG:78:1) = 'E') AND
016600111215     C                             TENTATIVO = 'PRIMO'
016700111215      *
016800111215     C                   MOVEL     'SECONDO2'    TENTATIVO
016900111215     c                   eval      w_o95lia=o95lia
017000111215      *
017100111215     C                   GOTO      RITENT
017200111215    2C                   ENDIF
017300111215     c
017400111215     c* Se il tisi95 non da errore ma lo da FNLV13, stampo lo stesso
017500111215    2C     O95ERR        ifeq      *BLANKS
017600111215     C     TENTATIVO     andeq     'PRIMO'
017700111215     c     o13err        andne     *blanks
017800111215     C                   EXSR      w_file
017900111219   x2c                   else
018000111219     c
018100111219     c* Se nessun errore verifico la linea di arrivo se diversa
018200111219     c*  dalla calcolata
018300111219    3c                   if        ilv10lna>0
018400111219     c*
018500111219    4c                   if        ilv10lna<>o95lna
018600111219     c                             or ilv10znc<>o95znc
018700111219     C                   EXSR      w_file
018800111219    4c                   endif
018900111219    3c                   endif
019000111219    2c                   endif
019100111215     C
019200111215    1c                   endif
019300111215    0c                   endif
019400111215     c
019500111215     c                   eval      Ilv10tla=WtipoLa
019600111215     c
019700111215     C* CHIUSURA PGM
019800111219    0C     ilv10tLa      IFEQ      ' '
019900111215     C                   RETURN
020000111215     C                   ELSE
020100111215     C*
020200111215     C                   CLEAR                   DSSI95
020300111215     C                   MOVEL     'C'           I95TLA
020400111215     C                   CALL      'TISI95R'
020500111215     C                   PARM                    DSSI95
020600111215     C*
020700111215     C                   SETON                                        LR
020800111215    0C                   ENDIF
020900111215      *
021000111215      *--------------------------------------------------------------*
021100111219      * w_file - scrittura DS   di output e file se richiesto
021200111215      *--------------------------------------------------------------*
021300111215     C     w_file        BEGSR
021400111215      *
021500111215     C                   MOVEL     O13MSG        olv10err
021600111215     C                   MOVEL     O13err        olv10cod
021700111215     c
021800111215     c                   if        w_o95lia<>*blANKS
021900111215     C                   MOVEL     w_o95lia      olv10liv
022000111215     c                   else
022100111215     c                   movel     o95lia        olv10liv
022200111215     c                   endif
022300111215      *
022400111215      * ALTRI DATI:
022500111215      *
022600111215      * SE NON SONO RIUSCITO A CORREGGERE PULISCO
022700111215      * I DATI IN SOSTITUZIONE, AGGIORNO MESSAGGIO DI ERRORE
022800111215      * PER SECONDO TENTATIVO
022900111215      *
023000111215     C     O13ROP        IFNE      'S'
023100111215     C     O13ROC        ANDNE     'S'
023200111215     C     O13ROL        ANDNE     'S'
023300111215     C     O13RON        ANDNE     'S'
023400111215     c                   if        olv10liv=*blanks or olv10liv = *zeros
023500111215     c                   eval      olv10liv='E'
023600111215     c                   endif
023700111215
023800111215     C     TENTATIVO     IFEQ      'SECONDO1'
023900111215     C                   CLEAR                   olv10err
024000111215     C                   EVAL      olv10err='C.A.P. o località obsoleti'
024100111215     C                   ENDIF
024200111215     c
024300111215     C                   ELSE
024400111215      *
024500111215      * ALTRIMENTI MUOVO CAMPI DA DS A FILE DI work
024600111215      * AGGIORNO MESSAGGIO DI ERRORE PER SECONDO TENTATIVO
024700111215      *
024800111215     C     TENTATIVO     IFEQ      'SECONDO1'
024900111215     C                   CLEAR                   olv10err
025000111215     C                   EVAL      olv10err='C.A.P. obsoleto'
025100111215     C                   ENDIF
025200111215      *
025300111215     C     TENTATIVO     IFEQ      'SECONDO2'
025400111215     C                   CLEAR                   olv10err
025500111215     C                   EVAL      olv10err='Provincia/CAP errato'
025600111215     C                   ENDIF
025700111215     c
025800111215     C     TENTATIVO     IFEQ      'PRIMO   '
025900111215     C                   EVAL      olv10err='CAP errato'
026000111215     c                   endif
026100111215      *
026200111215     C     O13ROP        IFEQ      'S'
026300111219     C                   MOVEL     O95PRV        olv10prvp
026400111215     C                   ENDIF
026500111215      *
026600111215     C     O13ROC        IFEQ      'S'
026700111219     C                   MOVEL     O95CAP        olv10capp
026800111215     C                   ENDIF
026900111215      *
027000111215     C     O13ROL        IFEQ      'S'
027100111219     C                   MOVEL     O95LOC        olv10locp
027200111215     C                   ENDIF
027300111215      *
027400111215     C     O13RON        IFEQ      'S'
027500111219     C                   MOVEL     O95NAR        olv10narp
027600111215     C                   ENDIF
027700111215      *
027800111215     C                   ENDIF
027900111215      *
028000111215      * completo il cod messaggio in base al messaggio
028100111215     c                   eval      n=%lookup(olv10err:err)
028200111215     c                   if        n>0
028300111215     c                   eval      olv10cod=erc(n)
028400111215     c                   endif
028500111215      *
028600111219     c* Impostata la Ds, se richiesta, scrivo il FILE
028700111219    1c                   if        ilv10file<>'N'
028800111219     c
028900111219     c                   clear                   wfeiv000
029000111219     c                   eval      eivccm=ilv10ccm
029100111219
029200111219    3c                   if        ilv10ccm>0
029300111219     c* reperisco unificante
029400111219     c                   clear                   tibs10ds
029500111219     c                   eval      d10tle='ST'
029600111219     c                   eval      d10paf='P'
029700111219     c                   eval      d10cod=ilv10ccm
029800111219     c                   call      'TIBS10R'
029900111219     c                   parm                    tibs10ds
030000111219     c
030100111219    4c                   if        d10cop=0
030200111219     c                   clear                   tibs10ds
030300111219     c                   eval      d10tle='ST'
030400111219     c                   eval      d10paf='F'
030500111219     c                   eval      d10cod=ilv10ccm
030600111219     c                   call      'TIBS10R'
030700111219     c                   parm                    tibs10ds
030800111219    4c                   endif
030900111219     c
031000111219    4c                   if        d10cop>0
031100111219     c                   eval      eivccu=d10cop
031200111219    4c                   endif
031300111219     c* richiamo tibs69r per recuperare la ragione sociale del cliente
031400111219     c                   clear                   DS_cnaco
031500111219     c                   clear                   DS_cnind
031600111219     c                   clear                   DS_cnclp
031700111219     c                   clear                   DS_fncls
031800111219     c                   z-add     ilv10ccm      i69kac
031900111219     c                   call      'TIBS69R'
032000111219     c                   parm                    tibs69ds
032100111219     c                   parm                    ds_cnaco
032200111219     c                   parm                    ds_cnind
032300111219     c                   parm                    ds_cnclp
032400111219     c                   parm                    ds_fncls
032500111219      *
032600111219     c                   eval      eivrsc=acorag
032700111219    3c                   endif
032800111219     c
032900111220     c                   eval      eivjob=ilv10job
033000111220     c                   eval      eivpru=knmus
033100111219     c                   eval      eivpou=dutpou
033200111219     c                   eval      eivsop=ilv10sop
033300111220     c                   eval      eivdrf=i_i95dat
033400111220     c                   eval      eivrmn=ilv10rmn
033500111219     c                   eval      eivrma=ilv10rma
033600111219     c                   eval      eivrsd=ilv10rsd
033700111219     c                   eval      eivind=ilv10ind
033800111220     c                   eval      eivnzd=i_i95nar
033900111220     c                   eval      eivcad=i_i95cap
034000111220     c                   eval      eivlod=i_i95loc
034100111220     c                   eval      eivprd=i_i95prv
034200111219     c                   eval      eivlna=ilv10lna
034300111219     c                   eval      eivznc=ilv10znc
034400111219     c
034500111219     c                   eval      eivnarp=olv10narp
034600111219     c                   eval      eivcapp=olv10capp
034700111219     c                   eval      eivlocp=olv10locp
034800111219     c                   eval      eivprvp=olv10prvp
034900111219     c                   eval      eivliv =olv10liv
035000111219     c                   eval      eivcod =olv10cod
035100111219     c                   eval      eiverr =olv10err
035200111219     c                   eval      eivlnap=o95lna
035300111219     c                   eval      eivzncp=o95znc
035400111220     c                   eval      eivlilp=o95lil
035500111219    2c                   select
035600111219     c                   when      eivlna=0
035700111219     c                   eval      eivdiv='N'
035800111219     c                   when      eivlna<>eivlnap
035900111219     c                   eval      eivdiv='L'
036000111219     c                   when      eivznc<>eivzncp
036100111219     c                   eval      eivdiv='Z'
036200111219    2c                   endsl
036300111219     c
036400111219     c                   write     wfeiv000
036500111219    1c                   endif
036600111219     c
036700111215     C                   ENDSR
036800111219     c*---------------------------------------------------------------------
036900111219     c     *INZSR        begsr
037000111219     c
037100111219     c     *dtaara       define    §azute        azuteds
037200111219     c     *dtaara       define    §datiute      ddatiute
037300111219     c                   in(E)     *dtaara
037400111219     c                   If        %error  or RSUT = *blanks
037500111219     c                   CLEAR                   tibs34ds
037600111219     c                   CALL      'TIBS34R'
037700111219     c                   PARM                    tibs34ds
037800111219     c                   in        *dtaara
037900111219     c                   EndIf
038000111219     c*
038100111219     C                   ENDSR
038200111219     c*
038300111215**
038400111215C.A.P. incongruente con provincia e/o localita'                               50
038500111215C.A.P. inesistente                                                            51
038600111215C.A.P. obsoleto                                                               52
038700111215CAP errato                                                                    54
038800111215Inserito un CAP generico di citta' che ha il Viario                           55
038900111215C.A.P. o località obsoleti                                                    31
039000111215Provincia/CAP errato                                                          40
