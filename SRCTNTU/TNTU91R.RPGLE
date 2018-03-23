000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200060105     H* TNTU91R *----------------------------------------------------*
000300060104     H* - LEGGE bolle arrivo per data arrivo     - fnarb
000400000000     H*--------------------------------------------------------------*
000500051214     ftntud01l  uf a e           k disk
000600060104     ffnarb29l  if   e           k Disk
000700060220     ffIar401l  if   e           k Disk
000800060104     ffiar501l  if   e           k Disk
000900060110     ffiar601l  if   e           k Disk
001000060104     ffiar901l  if   e           k Disk
001100060109     ftabel00f  if   e           k disk
001200051215     d
001300060110     D CBO             S              2    DIM(50)
001400060110     D ASS             S              2    DIM(50)
001500060110     D ASS2            S              2    DIM(50)
001600060111     D REC             S              2    DIM(50)
001700060109     D TFA             S              3  0 DIM(50)
001800060109     D TFAVAL          S              9  0 DIM(50)
001900060109     d
002000051214     dkpjba          e ds
002100051214     dtntu05ds       e ds
002200060103     dtntudtads      e ds
002300060105     ddsbl4a         e ds
002400060105     ddar5gen        e ds
002500060109     dtrul21ds       e ds
002600060109     dds3a           e ds
002700060320     D* DS PER FNLV80R - per verificare se tassativa
002800060320     D FNLV80DS      E DS
002900051215     d
003000060105     d Kdata           s                   Like(arbdam)
003100060109     d Kfgs            s                   Like(arblna)
003200060109     d savfgs          s                   Like(arblna)
003300051215     d Kmult           s                   Like(tudmult)
003400060508     d contaCS         s                   Like(tudqta)
003500060508     d contaddtno      s                   Like(tudqta)
003600060105     d contaddtsi      s                   Like(tudqta)
003700060105     d contaddtsino    s                   Like(tudqta)
003800060105     d contavalore     s                   Like(tudqta)
003900060105     d contaCA         s                   Like(tudqta)
004000060105     d contaAppunt     s                   Like(tudqta)
004100060105     d contaSuper      s                   Like(tudqta)
004200060105     d contaffd        s                   Like(tudqta)
004300060110     d contaSF         s                   Like(tudqta)
004400060110     d contaFImm       s                   Like(tudqta)
004500060323     d contaColli      s                   Like(tudqta)
004600060323     d conta           s                   Like(tudqta)
004700060110     d ktrc            s                   Like(ar4trc)
004800060105     D ktrd            s                   like(ar5trd) inz('GEN')
004900051215     d Kcli            s                   Like(tudcli) inz(0)
005000060109     d ktbkut          s                   like(tblkut) inz(1)
005100060109     d ktbcod          s                   like(tblcod) inz('3A')
005200051215     d Dataiso         s               d   datfmt(*iso)
005300060109     d Datafine        s               d   datfmt(*iso)
005400051220     d
005500900518     C***************************************************************
005600051214     c* Moltiplicatori calcolati:
005700060105     c*    030 - DDT SI + DDT NO
005800060105     c*    050 - DDT NO
005900060105     c*    052 - DDT SI
006000060109     c*    080 - assegnati segue fattura
006100060109     c*    082 - assegnati fattura immediata
006200060508     c*    085 - C/S in arrivo (no legati : cod.bolla = 'A')
006300060104     c*    160 - colli di valore
006400060323     c*    165 - colli arrivati
006500060105     c*    540 - telefonate per C/assegno
006600060105     c*    550 - telefonate per appuntamento
006700060105     c*    560 - telefonate per supermercati
006800060105     c*    570 - telefonate per Fermo deposito
006900060105     C***************************************************************
007000060110     c* Carico i cod bolla con C/Assegno  e in assegnato
007100060109     c                   exsr      carCBO
007200060109     c
007300060109    1c                   if        t05mult='160' or t05mult=*blanks
007400060109     c                   eval      kmult='160'
007500060109     c* loop sulla data
007600060109     c     *iso          movel     t05dti        dataiso
007700060109     c     *iso          movel     t05dtf        datafine
007800060109    2c                   dow       dataiso<=datafine
007900060109     c                   exsr      CANCMULT
008000060109     c
008100060109     c                   adddur    1:*D          dataiso
008200060109    2c                   enddo
008300060109    1c                   endif
008400060109     c
008500060105     c* Cerco i p.o. perchè la lf è per linea /data
008600060109    1c                   if        t05fil=0
008700051220     c
008800060105     c     kfgs          setgt     fnarb29l
008900060105     c                   read      fnarb29l
009000051220     c
009100051219     c                   else
009200060105     c     kfgs          setll     fnarb29l
009300060105     c     kfgs          reade     fnarb29l
009400060109    1c                   endif
009500051219     c
009600060109    1c                   dow       not %eof(fnarb29l)
009700060105     c                   z-add     arblna        kfgs
009800060508     c                   clear                   contaCS
009900060508     c                   clear                   contaddtno
010000060105     c                   clear                   contaddtsi
010100060105     c                   clear                   contaddtsino
010200060105     c                   clear                   contavalore
010300060105     c                   clear                   contaCA
010400060105     c                   clear                   contaAppunt
010500060105     c                   clear                   contaSuper
010600060105     c                   clear                   contaFFD
010700060109     c                   clear                   tfa
010800060109     c                   clear                   tfaval
010900060110     c                   clear                   contaSF
011000060110     c                   clear                   contafimm
011100060323     c                   clear                   contacolli
011200051215     c
011300051215     c* Imposta il giro sulla data
011400051215     c                   z-add     t05dti        kdata
011500060109    2c                   dow       kdata<=t05dtf
011600060105     c     karb          setll     fnarb29l
011700060105     c     karb          reade     fnarb29l
011800051220
011900060109    3c                   dow       not %eof(fnarb29l)
012000060111     c
012100060111     c* Spedizioni in assegnato
012200060111     c     arbcbo        lookup    ass                                    31
012300060111     c     arbcbo        lookup    ass2                                   32
012400060111     c                   if        *in31
012500060111     c                   eval      ktrc='1'
012600060111     c                   endif
012700060111     c                   if        *in32
012800060111     c                   eval      ktrc='2'
012900060111     c                   endif
013000060111     c
013100060111     c                   if        *in31 or *in32
013200060111     c     kar4          chain     fiar601l
013300060111     c                   if        %found(fiar601l)  and ar6dft>0
013400060111     c                   add       1             contaFIMM
013500060111     c                   else
013600060111     c                   add       1             contaSF
013700060111     c                   endif
013800060111     c                   endif
013900060111     c
014000060111     c* Per tutti gli altri conteggi, escludo i recuperi
014100060111     c     arbcbo        lookup    rec                                    31
014200060111     c                   if        not *in31
014300060105     c
014400060220     c* conto i ddt si/ddt no chainando fIar4
014500060109     c                   clear                   dsbl4a
014600060110     c                   eval      ktrc='A'
014700060220     c     kar4          chain     fIar401l
014800060220    4c                   if        %found(fIar401l)
014900060105     c                   movel     ar4not        dsbl4a
015000060109    4c                   endif
015100060105     c
015200060105     c* ddi si x stampa ldv
015300060105     c                   if        §b4abm='P' or §b4abm='Q'
015400060105     c                   add       1             contaddtsi
015500060105     c                   endif
015600060105     c* ddi no x stampa ldv
015700060105     c                   if        §b4abm='J' or §b4abm='W'
015800060105     c                   add       1             contaddtno
015900060105     c                   endif
016000060105     c
016100060105     c* ddt si + ddt no x smistamento ldv
016200060105     c                   add       1             contaddtsino
016300060508     c* C/S  cod.bolla = A
016400060508     c                   if        arbcbo='A '
016500060508     c                   add       1             contaCS
016600060508     c                   endif
016700060323     c* Colli arrivati
016800060323     c                   add       arbncl        contacolli
016900060105     c* Vedo se colli di valore
017000060105     c                   clear                   dar5gen
017100060105     c     kar5          chain     fiar501l
017200060105     c                   if        %found(fiar501l)
017300060105     c                   movel     ar5uni        dar5gen
017400060105     c                   endif
017500060105     c
017600060109    4c                   if        §ar5bva='V'
017700060109     c                   add       arbncl        contavalore
017800060109     c* Memorizzo anche per terminal di arrivo
017900060109    5c                   if        arbtfa<>arblna
018000060109     c                   z-add     1             yy
018100060109     c     arbtfa        lookup    tfa(YY)                                31
018200060109    6c                   if        not *in31
018300060109     c                   z-add     1             yy
018400060109     c     000           lookup    tfa(YY)                                31
018500060109     c                   eval      tfa(yy)=arbtfa
018600060109    6c                   endif
018700060109     c                   add       arbncl        tfaval(yy)
018800060109    5c                   endif
018900060109     c
019000060109    4c                   endif
019100060109     c
019200060109     c     arbcbo        lookup    cbo                                    31
019300060109    4c                   if        *in31
019400060109     c     kar9          chain     fiar901l
019500060109    5c                   if        %found(fiar901l)
019600060109      * NUOVO CONTROLLO C/ASSEGNO
019700060109     c                   clear                   trul21ds
019800060109     c                   eval      i21tla = 'L'
019900060109     c                   eval      i21cbo = arbcbo
020000060109     c                   eval      i21tsp = arbtsp
020100060109     c                   eval      i21lnp = arblnp
020200060109     c                   eval      i21nzm = arbnzm
020300060109     c                   eval      i21lna = arblna
020400060109     c                   eval      i21nzd = arbnzd
020500060109     c                   eval      i21tic = ar9tic
020600060109     c                   eval      i21imp = ar9cas
020700060109     c                   eval      i21div = ar9vca
020800060109     c                   eval      i21ute = knmus
020900060109     c                   eval      i21pgm = 'TNTU91R'
020901130827     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
020902130827     c                   if        %lookup(arbcbo:ass)>0
020903130827     c                   z-add     arbccm        i21ksc
020904130827     c                   if        i21ksc=0
020905130827     c                   z-add     8888          i21ksc
020906130827     c                   movel     arblnp        i21ksc
020907130827     c                   endif
020908130827     c                   else
020909130827     c                   z-add     arbksc        i21ksc
020910130827     c                   endif
020911130827
021000060109     c                   call      'TRUL21R'
021100060109     c                   parm                    trul21ds
021200060109
021300060109     c                   if        o21fca<> *blanks
021400060109     c                   add       1             contaCA
021500060109     c                   endif
021600060109     c
021700060109    5c                   endif
021800060109    4c                   endif
021900060109     c
022000060109     c                   if        arbtc1='A' or arbtc2='A'
022100060109     c                   add       1             contaAppunt
022200060109     c                   endif
022300060105     c
022400060109     c                   if        arbtc1='S' or arbtc2='S'
022500060320     c* EScludo le tassative
022600060320     c                   if        arbdcr>0 or arbhcr>0
022700060320     c                   clear                   fnlv80ds
022800060320     c                   eval      ilv80aas=arbaas
022900060320     c                   eval      ilv80lnp=arblnp
023000060320     c                   eval      ilv80nrs=arbnrs
023100060320     c                   eval      ilv80nsp=arbnsp
023200060320     c                   eval      ilv80mgs=arbmgs
023300060320     c                   eval      ilv80tc1=arbtc1
023400060320     c                   eval      ilv80tc2=arbtc2
023500060320     c                   eval      ilv80dcr=arbdcr
023600060320     c                   eval      ilv80hcr=arbhcr
023700060320     c                   eval      ilv80tcr=arbtcr
023800060320     c                   call      'FNLV80R'
023900060320     c                   parm                    kpjba
024000060320     c                   parm                    fnlv80ds
024100060320     c
024200060320     c                   if        olv80tat<>'S'
024300060109     c                   add       1             contaSuper
024400060320     c                   endif
024500060320     c                   else
024600060320     c                   add       1             contaSuper
024700060320     c                   endif
024800060320     c
024900060109     c                   endif
025000060109     c
025100060109     c                   if        arbffd='S'
025200060109     c                   add       1             contaFFD
025300060109     c                   endif
025400060111     c                   endif
025500060105     c
025600051219
025700060105     c     karb          reade     fnarb29l
025800060109    3c                   enddo
025900051219     c
026000060120     c                   if        t05mult=*blanks or  t05mult='570'
026100060109     c                   z-add     contaffd      conta
026200060109     c                   eval      kmult='570'
026300051215     c                   exsr      SCriviTUD
026400060120     c                   endif
026500060109     c
026600060120     c                   if        t05mult=*blanks or  t05mult='560'
026700060109     c                   z-add     contaSuper    conta
026800060109     c                   eval      kmult='560'
026900060109     c                   exsr      SCriviTUD
027000060120     c                   endif
027100060109     c
027200060120     c                   if        t05mult=*blanks or  t05mult='550'
027300060109     c                   z-add     contaAppunt   conta
027400060109     c                   eval      kmult='550'
027500060109     c                   exsr      SCriviTUD
027600060120     c                   endif
027700060109     c
027800060120     c                   if        t05mult=*blanks or  t05mult='540'
027900060109     c                   z-add     contaCa       conta
028000060109     c                   eval      kmult='540'
028100060109     c                   exsr      SCriviTUD
028200060120     c                   endif
028300060109     c
028400060120     c                   if        t05mult=*blanks or  t05mult='052'
028500060109     c                   z-add     contaDDTSI    conta
028600060109     c                   eval      kmult='052'
028700060109     c                   exsr      SCriviTUD
028800060120     c                   endif
028900060109     c
029000060120     c                   if        t05mult=*blanks or  t05mult='050'
029100060109     c                   z-add     contaDDTno    conta
029200060109     c                   eval      kmult='050'
029300060109     c                   exsr      SCriviTUD
029400060120     c                   endif
029500060109     c
029600060120     c                   if        t05mult=*blanks or  t05mult='030'
029700060109     c                   z-add     contaDDTSIno  conta
029800060109     c                   eval      kmult='030'
029900060109     c                   exsr      SCriviTUD
030000060120     c                   endif
030100060110     c
030200060120     c                   if        t05mult=*blanks or  t05mult='080'
030300060110     c                   z-add     contaSF       conta
030400060110     c                   eval      kmult='080'
030500060110     c                   exsr      SCriviTUD
030600060120     c                   endif
030700060109     c
030800060120     c                   if        t05mult=*blanks or  t05mult='082'
030900060110     c                   z-add     contaFIMM     conta
031000060110     c                   eval      kmult='082'
031100060109     c                   exsr      SCriviTUD
031200060120     c                   endif
031300060508     c
031400060508     c                   if        t05mult=*blanks or  t05mult='085'
031500060508     c                   z-add     contaCS       conta
031600060508     c                   eval      kmult='085'
031700060508     c                   exsr      SCriviTUD
031800060508     c                   endif
031900060508     c
032000060323     c                   if        t05mult=*blanks or  t05mult='165'
032100060323     c                   z-add     contaCOLLI    conta
032200060323     c                   eval      kmult='165'
032300060323     c                   exsr      SCriviTUD
032400060323     c                   endif
032500060110
032600060120     c                   if        t05mult=*blanks or  t05mult='160'
032700060110     c                   z-add     contaValore   conta
032800060110     c                   eval      kmult='160'
032900060110     c                   exsr      SCriviTUD
033000060109     c* Sommo anche i colli per il terminal di arrivo
033100060109     c                   eval      savfgs=kfgs
033200060109     c
033300060109     c                   z-add     1             yy
033400060109     c                   dow       tfa(YY)>0
033500060109     c                   z-add     tfa(yy)       kfgs
033600060109     c                   z-add     tfaval(yy)    conta
033700060109     c                   exsr      SCriviTUD
033800060109     c                   add       1             yy
033900060109     c                   enddo
034000060109     c
034100060109     c                   eval      kfgs=savfgs
034200060120     c                   endif
034300060105     c
034400060508     c                   clear                   contaCS
034500060508     c                   clear                   contaddtno
034600060105     c                   clear                   contaddtsi
034700060105     c                   clear                   contaddtsino
034800060105     c                   clear                   contavalore
034900060105     c                   clear                   contaCA
035000060105     c                   clear                   contaAppunt
035100060105     c                   clear                   contaSuper
035200060105     c                   clear                   contaFFD
035300060109     c                   clear                   tfa
035400060109     c                   clear                   tfaval
035500060110     c                   clear                   contaSF
035600060110     c                   clear                   contaFIMM
035700060323     c                   clear                   contaColli
035800051215     c
035900051219     c
036000051219     c* Aggiungo 1 giorno alla data
036100051219     c     *iso          movel     kdata         dataiso
036200051219     c                   adddur    1:*D          dataiso
036300051219     c                   movel     dataiso       kdata
036400060109    2c                   enddo
036500051215     c
036600051219     c                   if        t05fil=0
036700051220     c
036800060105     c     kfgs          setgt     fnarb29l
036900060105     c                   read      fnarb29l
037000051219     c                   else
037100051219     c* Se impostato un p.o. finito la lettura
037200051219     c                   leave
037300051219     c                   endif
037400051219     c
037500060109    1c                   enddo
037600051215     c
037700060111     c* richiamo pgm tntu91r per finire di scrivere i segue fattura
037800060111     c*  recuperi in assegnato
037900060111     c                   if        t05mult=*blanks or t05mult='080'
038000060111     c                   movea     rec           codbollarec     100
038100060111     c                   call      'TNTU90R'
038200060111     c                   parm                    kpjba
038300060111     c                   parm                    codbollarec
038400060111     c                   endif
038500060111     c
038600060103     c
038700060103     c* Se non richiesto un moltiplicatore o un p.o., aggiorno la tabella
038800060103     c*  TUP
038900060103     c                   if        t05mult=*blanks and t05fil=0
039000060103     c                   clear                   tntudtads
039100060103     c                   eval      §DTAazn = kcoaz
039200060103     c                   eval      §DTAdta = t05dtf
039300060103     c                   call      'TNTUDTAR'
039400060103     c                   parm                    tntudtads
039500060103     c                   endif
039600941216     C*
039700051123     C                   SETON                                        LR
039800991206     C**************************************************************************
039900051123     C     *INZSR        BEGSR
040000051214     c
040100051214     C     *ENTRY        PLIST
040200051214     C                   PARM                    kpjba
040300051214     c                   movel     kpjbu         tntu05ds
040400051123     c
040500060109     c     ktud2         Klist
040600060109     c                   Kfld                    kmult
040700060109     c                   Kfld                    dataiso
040800060109     C     KE2TAB        KLIST
040900060109     C                   KFLD                    KTBKUT
041000060109     C                   KFLD                    KTBCOD
041100060109     C     KAR9          KLIST
041200060109     C                   KFLD                    arbaas
041300060109     C                   KFLD                    arbLNP
041400060109     C                   KFLD                    arbNRS
041500060109     C                   KFLD                    arbNSP
041600060105     C     KAR4          KLIST
041700060105     C                   KFLD                    arbaas
041800060105     C                   KFLD                    arbLNP
041900060105     C                   KFLD                    arbNRS
042000060105     C                   KFLD                    arbNSP
042100060105     C                   KFLD                    KTRC
042200060105     C     KAR5          KLIST
042300060109     C                   KFLD                    arbAAS
042400060109     C                   KFLD                    arbLNP
042500060109     C                   KFLD                    arbNRS
042600060109     C                   KFLD                    arbNSP
042700060105     C                   KFLD                    ktrd
042800060105     c     karb          Klist
042900051219     c                   Kfld                    kfgs
043000051214     c                   Kfld                    kdata
043100051215     c     ktud          Klist
043200051215     c                   Kfld                    kmult
043300051215     c                   Kfld                    dataiso
043400051219     c                   Kfld                    kfgs
043500051215     c                   Kfld                    kcli
043600051214     c
043700060109     c* Considero il p.o. solo se richiesto un moltiplicatore <> da '160'
043800060109     c*  e diverso da blanks cioè tutti
043900060109     c                   if        t05fil>0  and t05mult<>*blanks  and
044000060109     c                             t05mult<>'160'
044100051219     c                   eval      kfgs=t05fil
044200060109     c                   else
044300060109     c                   clear                   t05fil
044400060109     c                   clear                   kfgs
044500051219     c                   endif
044600051219     c
044700051124    cc                   endsr
044800051215     C**************************************************************************
044900051215     c* Scrivo il file TNTUD00F
045000051215     C**************************************************************************
045100051215     C     SCriviTUD     BEGSR
045200051215     c     *iso          movel     kdata         dataiso
045300051215     c
045400060109     c                   if        conta>0
045500051215     c
045600051215     c     ktud          chain     tntud01l
045700051215     c
045800060109     c                   if        %found(tntud01l)  and kmult='160'
045900060111     c                   add       conta         tudqta
046000060109     c                   else
046100060111     c                   z-add     conta         tudqta
046200060109     c                   endif
046300051215     c
046400051215     c                   if        not %found(tntud01l)
046500051215     c                   eval      tuddta=dataiso
046600051215     c                   eval      tudmult=kmult
046700051219     c                   eval      tudfil=kfgs
046800051215     c                   clear                   tudcli
046900051215     c                   write     tntud000
047000051215     c                   else
047100051215     c                   update    tntud000
047200051215     c                   endif
047300051215     c                   endif
047400051215     c
047500051215     c                   ENDSR
047600060109      *-----------------------------------------------------------------*
047700060109     c* Carico cod bolla con C/A e in assegnato
047800060109      *-----------------------------------------------------------------*
047900060109     c     CarCBO        begsr
048000060109     C                   CLEAR                   II                4 0
048100060109     C                   CLEAR                   yy                4 0
048200060110     C                   CLEAR                   zz                4 0
048300060111     C                   CLEAR                   xx                4 0
048400060109     C     KE2TAB        SETLL     TABEL00F
048500060109     C     KE2TAB        READE     TABEL00F                               98
048600060109     C     *IN98         DOWEQ     *OFF
048700060109     C                   MOVEL     TBLUNI        DS3a
048800060109     C     §3afca        IFne      0
048900060109     C                   ADD       1             II
049000060109     C                   MOVEL     TBLKEY        CBO(II)
049100060109     C                   ENDIF
049200060109     c
049300060109     c                   movel     §3atb1        flgtb1            1
049400060110     c                   if        flgtb1='A'
049500060109     C                   ADD       1             yy
049600060109     C                   MOVEL     TBLKEY        ass(yy)
049700060109     c                   endif
049800060110     c                   if        §3atb2<>*blanks
049900060110     C                   ADD       1             zz
050000060110     C                   MOVEL     TBLKEY        ass2(zz)
050100060110     c                   endif
050200060111     c
050300060111     c                   if        §3arbl='R'
050400060111     C                   ADD       1             xx
050500060111     C                   MOVEL     TBLKEY        rec(xx)
050600060111     c                   endif
050700060111     c
050800060109     C     KE2TAB        READE     TABEL00F                               98
050900060109     C                   ENDDO
051000060109     C*
051100060109     C                   ENDSR
051200060109     C**************************************************************************
051300060109     c* Cancella il moltiplicatore se rielaborazione
051400060109     C**************************************************************************
051500060109     C     CANCMULT      BEGSR
051600060109     c     ktud2         setll     tntud01l
051700060109     c
051800060109     c                   setoff                                       31
051900060109     c                   dow       not *in31
052000060109     c     ktud2         delete    tntud01l                           31
052100060109     c                   enddo
052200060109     c
052300060109     c                   ENDSR
