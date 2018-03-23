000100980310     H DATEDIT(*DMY)
000200021024      *---------------------------------------------------------------------
000300021024      *  LEGGE TABELLA SCH E SOTTOMETTE LE AZIONI IN TABELLA
000400021024      *  SCHEDULANDOLE NEGLI ORARI IMPOSTATI
000500021104      *  Se l'orario è zero sottometto solo l' azioni in KSNOTTESK
000600021024      *  Le azioni con D§SCHESE = S sono sottomesse solo in sede
000700021024      *  Le azioni con D§SCHESE = P sono sottomesse solo nei p.o.
000800021024      *  Le azioni con D§SCHESE = E sono sottomesse nei p.o. e sede
000900021024      *---------------------------------------------------------------------
001000020403     Ftntbe01l  IF   E           K DISK
001100100308     Fazute01l  IF   E           K DISK
001200100308     fprtemail  o    f  132        printer  oflind(*inof)  usropn
001300011129      * ----------------------------------------------------------
001400100308
001500100308      // - Status
001600100308     d Psds           sds
001700100308     d   SDSpgm          *proc
001800100308     d   JobUser             254    263
001900100308
002000050218     D  ggmese         s              2  0 DIM(12) ctdata perrcd(12)
002100060426     D GIORSET         s              3    dim(7) ctdata perrcd(7)
002200950807     D KPJBA         E DS
002300011129     D UT§DSE0F      E DS
002400011129     D CNCR80        E DS
002500020403     D dsch          E DS
002600020403     D  ORE                   48    107    DIM(15)
002700060427     D  GGGiorn              125    131    DIM(7)
002800060427     D  Tuttigg              125    131
002900090320
003000090320     d TIBS02ds      e ds                  inz
003100020912
003200020912     d Tibs56Ds      e ds
003300020912     d  Poela                  7    756  0 dim(250)
003400121009     d
003500121009     d* DS x controllo riempimento schiere
003600121009     d trul0sds      e ds
003700020912
003800100308     d dmradan       e ds                  inz
003900100308     d trtcm1ds      e ds                  inz
004000100308      *    .§cm1mitt = Indirizzo e-mail del mittente
004100120302     d   §cm1mitt    e                     inz('ced@brt.it')
004200100308      *    ·§cm1dst  = Indirizzo e-mail del destinatario
004300120302     d   §cm1dst     e                     inz('elisa.sanfelici@brt.it')
004400100308      *    ·§cm1tips = Tipo lettera via e-mail:
004500100308      *     "LET" = testo allegato in corpo con logo
004600100308      *             (richiede righe libere iniziali per il logo)
004700100308      *     "COR" = testo integrato senza logo
004800100308      *             (non consente né UNDERLINE né HIGHLIGHT)
004900100308     d   §cm1tips    e                     inz('COR')
005000100308      *    ·§cm1po   = Filiale
005100100308     d   §cm1po      e                     inz('046')
005200100308      *    ·§cm1var  = Oggetto e-mail
005300100308     d   §cm1var     e                     inz('*OBJM*+
005400100308     d                                     SKEDULATORE: non trovato profilo -
005500100308     d                                     UTENTE')
005600100308      *    ·§cm1sts  = Stato
005700100308     d   §cm1sts     e                     inz(*off)
005800100308      *    ·§cm1idp  = Id processo
005900100308     d   §cm1idp     e                     inz('2')
006000100308
006100100308
006200011129      * DATA E ORA SCHEDULAZIONE PER KPJBA
006300980310     D SCHEDS          DS
006400980310     D  D1DATS                 1      6
006500050218     D  D1Dgg                  1      2  0
006600050218     D  D1Dmm                  3      4
006700980310     D  D1ORAS                 7     10
006800000630      *
006900000630     D DIGITS          C                   CONST('0123456789')
007000011129     D w0060           S              6  0
007100011129     D HHMM            S              4  0
007200011129     D ORARIO          S              4  0
007300011129     D XX              S              3  0
007400020912     d yy              s              3  0
007500060427     d gg              s              1  0
007600011129     D ElemORE         S              3  0
007700020912     d Wpo             s              3
007800100310     d Werror          s              3
007900020403     D Kcod            S                   LIKE(TBecod) inz('SCH')
008000150729     D azi             s             22    DIM(600) inz(*hival)
008100021024     D nazi            S              3  0
008200021024     D y               S                   like(nazi)
008300021024     D com3            S              3
008400021106     D libjob          S             10    inz
008500021106     D tiplib          S             10    inz
008600050218     D wgiorno         S              2  0
008700050316     d Datadmy         s               d   datfmt(*dmy)
008800050316     d wggmmaa         s              6
008900050316     d wgioset         s              1
009000051102     d Oraiso          s               t   timfmt(*iso)
009100100308     d $okmra          s              1n   inz(*off)
009200100308     d lenght          s             15  5
009300100308     d o_testo         s            132    inz
009400100308     d parmpo          s              3
009500100308     d qcmd            s            512    inz
009600100308     D* DEFINIZIONE COSTANTI
009700100308     d c_sede          c                   const('046')
009800100308      *  - comando di override al prtf
009900100308     d c_cmdovrprtf    c                   const('OVRPRTF +
010000100308     d                                           file(PRTEMAIL) +
010100100308     d                                           pagesize(66 132) +
010200100308     d                                           lpi(6) cpi(10) +
010300100308     d                                           ovrscope(*actgrpdfn) +
010400100308     d                                           ')
010500100308     d c_cmddltovr     c                   const('DLTOVR +
010600100308     d                                            file(PRTEMAIL) +
010700100308     d                                            lvl(*actgrpdfn)')
010800100308
010900090320
011000090320       //--------------------------------------------------------------
011100090320       //?Definizione prototipi procedure.                             ?
011200090320       //--------------------------------------------------------------
011300090320
011400090320       // - Ricerca e controllo tabelle
011500090320      /copy gaitrasrc/srcprotopr,TIBS02R
011600090320
011700000630      * ----------------------------------------------------------
011800021024     c* carico la schiera delle azioni ordinandole per priorità
011900150729     c                   clear                   troppiel          1
012000020403     C     kcod          SETLL     tntbe01l
012100021024     c                   do        *hival
012200020403     C     kcod          READE     tntbe01l
012300021024     c                   if        %EOF(tntbe01l)
012400021024     c                   leave
012500021024     c                   end
012600021108      *    ometto le annullate
012700021108     c                   if        tbeatb <> ' '
012800021028     c                   iter
012900021028     c                   end
013000021108      *    elaboro record mio sistema informativo
013100021108     c                   if        tbesif <> knsif
013200021108     c                   iter
013300021108     c                   end
013400021024     c*
013500021024     c                   movel(p)  tbeuni        dsch
013600121008     C*    ometto le sospese
013700121008     c                   if        D§SCHSosp = 'S'
013800121008     C                   CLEAR                   DSCH
013900121008     c                   iter
014000121008     c                   end
014100121009     c*
014200121009     c                   add       1             nazi
014300121009     c* h1 ind. errore schiera AZI raggiunge il limite massimo
014400150729     c                   if        nazi > 600
014500150729     c***                seton                                        h1
014600150729     c                   eval      troppiel='S'
014700121009     c                   leave
014800121009     c                   end
014900090729     c* carico la schiera AZI con priorità e codice azione e cod.param.
015000021108     c                   eval      azi(nazi)=d§schpri + %subst(tbeke1: 1: 4)
015100090729     c                                                + TBEke2
015200021024     c                   enddo
015300021024     c* ordino la schiera
015400021024     c                   sorta     azi
015500000630      *
015600021024     c                   do        nazi          y
015700021024     c                   if        azi(y)= *hival
015800021024     c                   leave
015900021024     c                   end
016000021024     c                   eval      tbeke1 = %subst(azi(y): 4: 4)
016100090729     c                   eval      tbeke2 = %subst(azi(y): 8: 15)
016200021028     C     ktab          setll     tntbe01l
016300021028     c                   do        *hival
016400021028     C     ktab          reade     tntbe01l
016500021028     c                   if        %eof(tntbe01l)
016600021028     c                   leave
016700021028     c                   end
016800021028      *    elaboro record mio sistema informativo
016900021028     c                   if        tbesif <> knsif
017000021028     c                   iter
017100021028     c                   end
017200011129      *    elaboro non annullati
017300020403     c                   IF        tbeatb = *blanks
017400020403     C                   MOVEL     tbeuni        dsch
017500020912      * Se in tabella non è indicato l'utente non elabora
017600020912If  4c                   If        d§schute <> *Blanks
017700011129      *    verifico se l'azione è eseguibile
017800020912      *    da p.o. o da sede o da sede as unico
017900020403     c                   IF        d§schese = 'E'  or
018000020912     c                             (d§schese = 'S'  and  $As4 = 'S') or
018100020920     c                             (d§schese = 'P'  and  $As4 = 'P')
018200020403      *    lancio solo le azioni che hanno la frequenza richiesta
018300020403     c                   if        d§schfre = $fre
018400020912      * controllo se devo lanciare l'azione 1 volta o n.volte
018500020912If  7c                   If        d§schtps <> *Blanks
018600020912     c                   ExSr      ElabM
018700020912   x7c                   Else
018800011129     c                   exsr      ELABORA
018900020912    7c                   EndIf
019000020403     c                   endif
019100011129     c                   ENDIF
019200020912    4c                   EndIf
019300021024     c                   ENDif
019400021024     c                   enddo
019500021028     c                   enddo
019600100308
019700100308     c                   exsr      SR_END
019800100308     c
019900150729     c                   if        troppiel='S'
020000150729     c                   seton                                        h1
020100150729     c                   endif
020200150729     c
020300011129     C                   EVAL      *INLR = *ON
020400011129      * ----------------------------------------------------------
020500050218      *  Sottometto 1 sola volta l'azione
020600050218      * ----------------------------------------------------------
020700050218     C     ELABORA       BEGSR
020800011129      *
020900050218     c                   Eval      Knmus = d§schute
021000100308     c* Verifico se esiste l'utente: sse non esiste do errore
021100100308     c     knmus         chain     azute01l
021200100308     c                   if        %found(azute01l)
021300050218     c                   exsr      SOTTOMETTE
021400100308     c                   else
021500100310     c                   eval      werror='UTE'
021600100308     c                   exsr      InviaMSG
021700100308     c                   endif
021800980310      *
021900011129     C                   ENDSR
022000020912      * ---------------------------------------------------------------
022100020912      *  Sottometto n volte l'azione quanti sono i terminal di partenza
022200020912      * ---------------------------------------------------------------
022300020912     c     ElabM         BegSr
022400020912      * Schiera dei terminal
022500050218    1c                   Do        250           yy
022600050218     c
022700020912     c                   If        Poela(yy) = *zeros
022800020912     c                   Leave
022900020912     c                   EndIf
023000050218     c
023100050218     c                   Movel     Poela(yy)     Wpo
023200050218     c                   Eval      Knmus = d§schute
023300050218     c                   Eval      %Subst(Knmus:7:3) = Wpo
023400050218     c
023500100308     c* Verifico se esiste l'utente: sse non esiste do errore
023600100308     c     knmus         chain     azute01l
023700100308     c                   if        %found(azute01l)
023800050218     c                   EXSR      SOTTOMETTE
023900100308     c                   else
024000100310     c                   eval      werror='UTE'
024100100308     c                   exsr      InviaMSG
024200100308     c                   endif
024300020912
024400050218    1c                   EndDo
024500020912      *
024600020912     C                   ENDSR
024700050218      * ---------------------------------------------------------------
024800050218      *  Sottometto l'azione
024900050218      * ---------------------------------------------------------------
025000050218     c     SOTTOMETTE    begsr
025100060427     c                   clear                   welanotte         1
025200050218     C                   CLEAR                   KBUFF
025300050218     C                   MOVE      UDATE         D1DATS
025400050218     C                   MOVEL     tbeke1        KCOAZ
025500060118     C                   CLEAR                   Kqebi
025600060427     c
025700060427     c* Per Frequenza   G I O R N A L I E R A   sottometto
025800060427     c* solo se il giorno della settimana è previsto
025900100104    1c                   if        $fre='G'
026000060427     c                   clear                   welabora          1
026100060427     c
026200100104    2c                   if        tuttiGG=*blanks
026300060427     c                   eval      welabora='S'
026400060427     c                   else
026500060427     c                   Movel     d1dats        wggmmaa
026600060427     c                   Call      'XGIOSE1'
026700060427     c                   Parm                    wggmmaa
026800060427     c                   Parm                    wgioset
026900060427     c     wgioset       lookup    ggGiorn                                30
027000100104    3c                   if        *in30
027100060427     c                   eval      Welabora='S'
027200100104    3c                   endif
027300100104    2c                   endif
027400060427     c
027500060427     c* se non è da elaborare, non sottometto l'azione
027600100104    2c                   if        welabora=' '
027700060427     c                   goto      endSottom
027800100104    2c                   endif
027900100104    1c                   endif
028000060427     c
028100050218     C* J O B Q
028200050218      * se impostata passo la jobq
028300050218    1c                   if        d§schjob <> *blanks
028400050408     c                   eval      kqebi = d§schjob
028500050218     c* se jobq è KSNOTTESK controllo che esista altrimenti imposto KSNOTTE
028600050218    2c                   if        d§schjob = 'KSNOTTESK'
028700050218     c                   clear                   libjob
028800050218     c                   eval      tiplib = '*JOBQ'
028900050218     c                   call      'TRUL29C'
029000050218     c                   parm                    d§schjob
029100050218     c                   parm                    tiplib
029200050218     c                   parm                    libjob
029300050218     c* se la libreria che ritorna è ' ' vuol dire che non esiste KSNOTTESK
029400050218    3c                   if        libjob = *blanks
029500050218     c                   eval      kqebi = 'KSNOTTE'
029600050218     c                   else
029700050218     c                   eval      kqebi = d§schjob
029800050218    3c                   endif
029900050218    2c                   endif
030000050218    1c                   endif
030100060427     c
030200060427     c* Per Frequenza   S E T T I M A N A L E   sottometto
030300060427     c*  nel giorno della settimana previsto: ignoro il giorno se
030400091221     c*  l'elaborazione è in notte e non è "6" o "7"
030500060427     c     'NOTTE'       scan      d§schjob                               30
030600091221     c*
030700100104    1c                   if        *in30 and (d§schg1s='6' or
030800091221     c                             d§schg1s='7')
030900100104     c* Se presente almeno un orario, non lo considero come notte
031000100104    2C                   DO        ElemORE       xx
031100100104      * controllo sia numerico
031200100104    3c                   IF        %subst(ore(xx):1:1) >= '0'  and
031300100104     c                             %subst(ore(xx):2:1) >= '0'  and
031400100104     c                             %subst(ore(xx):3:1) >= '0'  and
031500100104     c                             %subst(ore(xx):4:1) >= '0'
031600100104      *
031700100104     C                   MOVE      ORE(xx)       ORARIO
031800100104     c
031900100104    4c                   if        orario>0
032000091221     c                   setoff                                       30
032100100104     c                   leave
032200100104    4c                   endif
032300100104    3c                   endif
032400100104    2c                   enddo
032500100104     c
032600100104    1c                   endif
032700091221     c
032800060428    1c                   if        not *in30
032900060428    2c                   if        $fre='S'
033000060427     c                   Movel     d1dats        wggmmaa
033100060427     c                   Call      'XGIOSE1'
033200060427     c                   Parm                    wggmmaa
033300060427     c                   Parm                    wgioset
033400060427     c
033500060427     c* Prevedo sottomissioni da WgioSET fino a 7. Escludo se giorno già
033600060427     c*  passato della settimana che rientrerà con la prossima
033700060427     c*  sottomissione settimanale
033800060427    3c                   if        d§schg1s<>' ' and d§schg1s<wgioset
033900060427     c                             or d§schg1s=' '
034000060427     c                   goto      endsottom
034100060427    3c                   endif
034200060427     c
034300060427    3c                   if        d§schg1s>wgioset
034400060427     c                   movel     d§schg1s      pp                1 0
034500060427     c                   movel     wgioset       mm                1 0
034600060427     c     pp            sub       mm            gg                1 0
034700060427     c                   movel     d1dats        wdata
034800060427     c     *dmy          Movel     wdata         datadmy
034900060427     c                   adddur    gg:*D         datadmy
035000060427     c     *dmy          movel     datadmy       wdata             6 0
035100060427     c                   movel     wdata         d1dats
035200060427    3c                   endif
035300060428    2c                   endif
035400091228
035500100104   x1c                   else
035600100104     c                   eval      welanotte='S'
035700060427    1c                   endif
035800060426     c
035900050218     C* Frequenza   "M E N S I L E"
036000050218     c* devo impostare il gg di sottomissione: se > dell'ultimo gg del mese
036100050218     c*   imposto l'ultimo del mese
036200050218    1c                   if        $fre='M'
036300050218     c
036400050218     c                   movel     d1dmm         wmm               2 0
036500050218    2c                   if        d§schggm=0
036600050218     c                   eval      wgiorno=d1dgg
036700050218   x2c                   else
036800050218    3c                   if        d§schggm>ggmese(wmm)
036900050218     c                   eval      wgiorno=ggmese(wmm)
037000050218     c                   else
037100050218     c                   eval      wgiorno=d§schggm
037200050218    3c                   endif
037300050218     c* Se il giorno è già trascorso, imposto = a oggi
037400050218    3c                   if        D1Dgg>wgiorno
037500050218     c                   eval      wgiorno=D1Dgg
037600050218    3c                   endif
037700050218    2c                   endif
037800050218     c
037900050218     c                   movel     wgiorno       d1dats
038000050316     c
038100050316     c* se il giorno impostato è domenica, addiziono 1 al giorno
038200050316     c                   Movel     d1dats        wggmmaa
038300050316     c                   Call      'XGIOSE1'
038400050316     c                   Parm                    wggmmaa
038500050316     c                   Parm                    wgioset
038600050316     c
038700060427    5c                   if        wgioset='7'
038800050316     c                   movel     d1dats        wdata
038900050316     c     *dmy          Movel     wdata         datadmy
039000050316     c                   adddur    1:*D          datadmy
039100050316     c     *dmy          movel     datadmy       wdata             6 0
039200050316     c                   movel     wdata         d1dats
039300050218    1c                   endif
039400050316    1c                   endif
039500050218      *
039600100104      *  ELABORO GLI ORARI DI SCHEDULAZIONE
039700050218    1C                   DO        ElemORE       xx
039800050218      * controllo sia numerico
039900050218    2c                   IF        %subst(ore(xx):1:1) >= '0'  and
040000050218     c                             %subst(ore(xx):2:1) >= '0'  and
040100050218     c                             %subst(ore(xx):3:1) >= '0'  and
040200050218     c                             %subst(ore(xx):4:1) >= '0'
040300050218      *
040400050218     C                   MOVE      ORE(xx)       ORARIO
040500060427     c
040600060427      *  SCHEDULA   "Settimanale" in notte senza orario: ok
040700060427     c*                           ma non schedulato, non serve
040800060427    3c                   select
040900060428    3c                   when      welanotte='S'
041000100104     c* imposto la data di schedulazione ma non l'ora
041100060428     c                   if        $fre='M'
041200060428     c                   clear                   d1oras
041300060428     C                   MOVEL     SCHEDS        KBUFF
041400060428     c                   endif
041500060427     c
041600060428     c* Per "Settimanale"  e "Mensile" se non c'e' orario non elaboro
041700060427    3c                   when      $fre='S'and orario<=*zeros
041800060428     c                   iter
041900060428    3c                   when      $fre='M'and orario<=*zeros
042000060427     c                   iter
042100060427     c
042200060427      *  SCHEDULA   "Giornaliera"
042300050218      *  Se orario impostato non ancora trascorso sottometto azione
042400050218      *  Per verificare orario utilizzo ora job + 5 minuti per evitare
042500050218      *  di sottomettere un azione in una ora già trascorsa
042600060427    3c                   when      $fre='G'
042700050218    4C                   IF        ORARIO <> *zeros  and  orario > HHMM
042800050218     C                   MOVE      ORARIO        D1ORAS
042900050218      * MUOVO data e ora NEL CAMPO DELLA KPBJA UTILIZZATO X LA SCHEDULAZIONE
043000050218     C                   MOVEL     SCHEDS        KBUFF
043100060427   x4c                   else
043200060427     c* Se non c'e' orario, esco
043300060427    5c                   if        orario<=*zeros
043400060427     c                   iter
043500060427    5C                   ENDIF
043600100310     c
043700100310     c* se orario già trascorso, invio msg di errore
043800100310     c                   if        orario<HHMM
043900100310     c                   eval      werror='ORA'
044000100310     c                   exsr      Inviamsg
044100100310     c                   endif
044200100310     c
044300060427    4C                   ENDIF
044400060427   x3c                   other
044500050218     c
044600060427      *  SCHEDULA   "Mensile"  e  "Settimanale" non in notte
044700050218      *  Se orario impostato già trascorso, lo imposto = adesso
044800050218     c                   movel     D1DATS        w0060
044900060427    4C                   IF        orario < HHMM and  w0060=udate
045000050218     c                   eval      orario=hhmm
045100050218    4c                   endif
045200050218     c
045300050218     C                   MOVE      ORARIO        D1ORAS
045400050218      * MUOVO data e ora NEL CAMPO DELLA KPBJA UTILIZZATO X LA SCHEDULAZIONE
045500050218     C                   MOVEL     SCHEDS        KBUFF
045600060427    3C                   ENDsl
045700050218     c
045800060427     c* l'ora non c'è solo per i lavori settimanali in notte
045900060427    3C***                IF        ORARIO = *zeros and d§schfre <>'S'
046000060427     c***                iter
046100060427    3c***                end
046200060427     c
046300090320      /free
046400090320         clear  KPJBU;
046500090320         // - Verifica se previsti parametri in tab. "SCK"
046600090320         //   per l'azione schedulata
046700090320         if  TBEke2 > *zero;
046800100308           clear TIBS02ds;
046900100308           T02mod = 'C';
047000100308           T02cod = 'SCK';
047100090320           T02sif = TBEsif;
047200090320           T02ke1 = TBEke2;
047300090320           clear  T02ke2;
047400090320           TNTBE_RicercaControllo ( kpjba : tibs02ds );
047500090320           if  T02err = *blanks;
047600090320             KPJBU = T02uni;
047700090320           endif;
047800090320         endif;
047900090320      /end-free
048000090320      *
048100050218     C                   CALL      'BCH10'
048200050218     C                   PARM                    KPJBA
048300060427     c
048400060427     c* se lavoro settimanale in notte, esco dal loop delle ore
048500060427    3C***                IF        ORARIO = *zeros and d§schfre ='S'
048600060427     c
048700060427    3C                   IF        welanotte='S'
048800050218     c                   leave
048900060427    3c                   endif
049000050218      *
049100050218    2C                   ENDIF
049200050218    1C                   ENDDO
049300060427     c     ENDSottom     ENDSR
049400000630      *---------------------------------------------------------
049500980310     C     *INZSR        BEGSR
049600011129      *
049700980310     C     *ENTRY        PLIST
049800980310     C                   PARM                    KPJBA
049900020403     c                   parm                    $fre              1
050000020912     c                   parm                    $As4              1
050100021024     c*
050200021024     c     ktab          klist
050300021024     c                   kfld                    kcod
050400021024     c                   kfld                    tbeke1
050500090729     c                   kfld                    tbeke2
050600980310      *
050700011129     C                   TIME                    W0060
050800051102     c     *iso          movel     w0060         oraiso
050900051102     c                   adddur    5:*mn         oraiso
051000051102     C     *iso          MOVEL     oraiso        w0060
051100051102     C                   MOVEL     w0060         HHMM
051200051102     C****               add       5             HHMM
051300011129     c                   eval      codut = 1
051400011129      * ricavo SIMFEL
051500011129     C                   CALL      'X§PARUT'
051600011129     C                   PARM                    UT§DSE0F
051700011129     C                   MOVEL     REC80         CNCR80
051800011129      * memorizzo elementi schiera ore
051900011129     c                   eval      ElemORE = %ELEM(ORE)
052000020912      * memorizzo i Terminal di partenza del p.o.
052100020912     c                   Clear                   Tibs56Ds
052200020912     c                   Eval      I56ppo = Simfel
052300020912     c                   Eval      I56mod = 'TPS'
052400020912     c                   Call      'TIBS56R'
052500020912     c                   Parm                    Tibs56Ds
052600980310      *
052700980310     C                   ENDSR
052800100308      *
052900100308     c* Invio messaggio di errore alla sede per UTENTE non trovato -----------*
053000100308     c     INVIAMSG      BEGSR
053100100308
053200100308      * Reperimento tab. "MRA" se non già recuperata
053300100308     c                   if        not $okmra
053400100308     c                   clear                   dmradan
053500100308     c                   clear                   tibs02ds
053600100308     c                   eval      t02mod = 'C'
053700100308     c                   eval      t02sif = knsif
053800100308     c                   eval      t02cod = 'MRA'
053900100308     c                   eval      t02ke1 = sdspgm
054000100308     c                   call      'TIBS02R'
054100100308     c                   parm                    kpjba
054200100308     c                   parm                    tibs02ds
054300100308     c                   if        t02err = *blanks
054400100308     c                   eval      dmradan = t02uni
054500100308     c                   eval      $okmra = *on
054600100308     c                   endif
054700100308     c                   endif
054800100308
054900100308      * Override al file di stampa ed apertura dello stesso
055000100308     c                   if        not %open(prtemail)
055100100308     c                   exsr      sr_openprtf
055200100308     c                   endif
055300100308
055400100308      * Stampa errore
055500100310     c                   if        werror='UTE'
055600100310     c                   eval      o_testo='AZIONE : '+ (%subst(azi(y):4:4))+
055700100310     c                             '    Utente non trovato  : ' + knmus
055800100310     c                   else
055900100310     c                   eval      o_testo = 'AZIONE : '+ (%subst(azi(y):4:4))+
056000100310     c                             '    Orario già trascorso: '+
056100100310     c                             (%editw(orario:'  :  '))
056200100310     c                   endif
056300100310     c                   except    prtdet
056400100308
056500100308      * Stampa una riga vuota
056600100308     c                   clear                   o_testo
056700100308     c                   except    prtdet
056800100308
056900100308     C                   ENDSR
057000100308      *------------------------------------------------------------------------*
057100100308      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
057200100308      *  + STAMPA INIZIO MAIL
057300100308      *------------------------------------------------------------------------*
057400100308     c     sr_openprtf   BEGSR
057500100308
057600100308      * Override al file di stampa
057700100308     c                   exsr      sr_override
057800100308
057900100308     c                   open      prtemail
058000100308
058100100308      * Stampa una testata se NON è richiesta la e-mail
058200100308     c                   if        §mradreg = *blanks
058300100308     c                   eval      o_testo = jobuser + ' - ' + sdspgm
058400100308     c                             + ' - ' + %editc( *date : 'Y' )
058500100308     c                             + ' - *REM* ' + %subst(§cm1var : 7 : 70)
058600100308     c                   except    prtdet
058700100308     c                   clear                   o_testo
058800100308     c                   except    prtdet
058900100308     c                   except    prtdet
059000100308     c                   endif
059100100308
059200100308      * Stampa testo iniziale
059300100308     c                   eval      o_testo = 'Utente non trovato per skedulazio+
059400100308     c                                        ne lavori'
059500100308     c                   except    prtdet
059600100308
059700100308      * Stampa una riga vuota
059800100308     c                   clear                   o_testo
059900100308     c                   except    prtdet
060000100308
060100100308     c                   ENDSR
060200100308      *------------------------------------------------------------------------*
060300100308      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
060400100308      *  + STAMPA INIZIO MAIL
060500100308      *------------------------------------------------------------------------*
060600100308     c     sr_override   begsr
060700100308
060800100308     c                   reset                   trtcm1ds
060900100308
061000100308     c                   if        §mradreg <> *blanks
061100100308     c                   eval      §cm1mitt = %trim(§mradmitt)
061200100308     c                   eval      §cm1dst  = %trim(§mraddest)
061300100308     c                   eval      §cm1tips = §mradreg
061400100308     c                   eval      §cm1po   = c_sede
061500100308     c                   eval      §cm1var  = '*OBJM*' + §mraddes
061600100308     c                   eval      §cm1idp  = §mradidpro
061700100308     c                   eval      qcmd = c_cmdovrprtf
061800100308     c                             + ' outq(' + %trim(§mradoutqi) + ')'
061900100308     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
062000100308     c                   else
062100100308     c                   eval      qcmd = c_cmdovrprtf
062200100308     c                   endif
062300100308
062400100308     c                   eval      lenght = %size(qcmd)
062500100308     c                   call(e)   'QCMDEXC'
062600100308     c                   parm                    qcmd
062700100308     c                   parm                    lenght
062800100308
062900100308     c                   endsr
063000100308      *------------------------------------------------------------------------*
063100100308      * ROUTINE FINALE
063200100308      *------------------------------------------------------------------------*
063300100308     c     SR_END        BEGSR
063400121009     C*
063500121009     c* Controllo riempimento schiera
063600121009     c                   clear                   trul0sds
063700121009     c                   eval      i0sski='AZI'
063800121009     c                   eval      i0sele=%elem(azi)
063900121009     c                   eval      i0spie=nAzi
064000121009     c                   eval      i0sfile='Az.schedul'
064100121009     c                   eval      i0ssif=knsif
064200121009     c                   eval      i0spgm='TNSY99R'
064300121009     c                   movel     trul0sds      kpjbu
064400121009     c                   call      'TRUL0SR'
064500121009     c                   parm                    kpjba
064600100308
064700100308     c                   if        %open(prtemail)
064800100308
064900100308      * Chiusura dello spool?
065000100308     c                   eval      o_testo = sdspgm
065100100308     c                   except    prtdet
065200100308     c                   clear                   o_testo
065300100308     c                   except    prtdet
065400100308     c                   eval      o_testo = '***   Fine Lista   ***'
065500100308     c                   except    prtdet
065600100308
065700100308     c                   close     prtemail
065800100308
065900100308      * Eliminazione overflow?
066000100308     c                   eval      lenght = %size(qcmd)
066100100308     c                   eval      qcmd = c_cmddltovr
066200100308     c                   call(e)   'QCMDEXC'
066300100308     c                   parm                    qcmd
066400100308     c                   parm                    lenght
066500100308
066600100308     c                   endif
066700100308
066800100308      *
066900100308      /free
067000100308       clear  TIBS02ds;
067100100308       T02tla = 'C';
067200100308       TNTBE_RicercaControllo ( kpjba : tibs02ds );
067300100308      /end-free
067400100308     c                   ENDSR
067500100308
067600100308     oprtemail  e            prtdet      1
067700100308     o                       o_testo
067800050218**
067900050218312831303130313130313031
068000060426** GIORSET-  GIORNI DELLA settimana                                          *
068100060426LUNMARMERGIOVENSABDOM
