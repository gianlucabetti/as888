000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200011011      *
000300020207      *  Routine recupero tariffa di cartello
000400020207      * -------------------------------------
000500020207
000600020207     fazorg01l  if   e           k disk    usropn
000700011019
000800020619     d fil             s              3  0 dim(50)                              filiale
000900020619     d ntw             s              3    dim(50)                              Network
001000021115     d msg             s             78    dim(10) ctdata perrcd(1)
001100020207
001200020211     d trul27ds      e ds
001300020211     d fnlv51ds      e ds
001400020208     d og143         e ds
001500020208     d dtca          e ds
001600021115     d ddfe          e ds
001700011029     d tibs02DS      e ds
001800011030     d kpjba         e ds
001900020211     d ut§dse0f      e ds
002000020211     d  tcu                  398    697
002100020211     d                                     dim(50)
002200020211     d  kcu                  698    847p 0
002300020211     d                                     dim(50)
002400020211     d                                     packeven
002500011030
002600020211     d tcuds           ds
002700020211     d  f1                     1      1
002800020211     d  f3                     3      3
002900020211     d  f2                     2      2
003000020211     d  f4                     4      4
003100020211     d  f56                    5      6
003200020211
003300020211     d x               s              2  0
003400020211     d xx              s              3  0
003500020208     d yy              s              3  0
003600020208     d pocli           s              3  0
003700020214     d wi27tln         s                   like(i27tln)
003800021115     d Wctb            s              2  0
003900120920     D Volta1          s               n
004000011019
004100011019      *----------------------------------------------------------------
004200011019
004300011029     c     *entry        plist
004400020211     c                   parm                    trul27ds
004500020214
004600020214      * non posso avere i27lna e i27tln valorizzati
004700020214      * la linea di arrivo vince sul tipo linea di arrivo
004800020214     c                   eval      wi27tln = i27tln
004900020214     c                   if        wi27tln <> *blanks and i27lna <> *zeros
005000020214     c                   clear                   wi27tln
005100020214     c                   endif
005200011019
005300020220     c                   eval      *in30 = *off
005400020220     c                   eval      *in31 = *off
005500020211     c                   clear                   o27ksc
005600020211     c                   clear                   o27ctr
005700020211     c                   clear                   o27prg
005800020213     c                   clear                   o27ntw
005900020218     c                   clear                   o27fie
006000020211     c                   clear                   o27err
006100020211     c                   clear                   o27msg
006200011031
006300011031      * Controllo validità tipo lancio
006400020211     c                   if        i27tla <> 'C' and i27tla <> 'L' and
006500020211     c                             i27tla <> *blanks
006600020211     c                   clear                   i27tla
006700011031     c                   endif
006800011019
006900011019      * Se Tipo lancio 'C' --> solo chiusura file
007000020211     c                   if        i27tla <> 'C'
007100011029     c                   exsr      elabora
007200011029     c                   endif
007300020215
007400020215      * Se tipo lancio 'C' richiamo FNLV51R per chiusura file
007500020215     c                   if        i27tla = 'C'
007600020215     c                   clear                   fnlv51ds
007700020215     c                   eval      d51tla = i27tla
007800020215     c                   call      'FNLV51R'
007900020215     c                   parm                    fnlv51ds
008000020215     c                   endif
008100011011
008200020211     c                   if        i27tla <> *blanks
008300011031     c                   eval      *inlr = *on
008400011029     c                   else
008500011031     c                   eval      *inrt = *on
008600011029     c                   endif
008700011011
008800011011      *----------------------------------------------------
008900011019      * ELABORAZIONE
009000011011      *----------------------------------------------------
009100011029     c     elabora       begsr
009200020208
009300120920     C* eseguo questo reperimento solo la prima volta
009400120920     C                   IF        Volta1 <> *on
009500120920     C                   EVAL      Volta1 =  *on
009600020208      * aggancio la tabella TCA
009700020208     c                   clear                   dtca
009800020208     c                   clear                   tibs02ds
009900020208     c                   movel     'C'           t02mod
010000020208     c                   movel     knsif         t02sif
010100020208     c                   movel     'TCA'         t02cod
010200020208     c                   movel(p)  '1'           t02ke1
010300020208     c                   call      'TIBS02R'
010400020208     c                   parm                    kpjba
010500020208     c                   parm                    tibs02ds
010600020208
010700020208     c                   if        t02err = *blanks
010800020208     c                   movel     t02uni        dtca
010900020208     c                   endif
011000020208
011100020208     c                   if        t02err = 'E'
011200020211     c                   eval      o27err = 'E'
011300020211     c                   eval      o27msg = msg(1)
011400020208     c                   goto      endela
011500020208     c                   endif
011600021115
011700021115      * aggancio la tabella DFE
011800021115     c                   clear                   ddfe
011900021115     c                   clear                   tibs02ds
012000150821     c                   movel     'L'           T02tla
012100021115     c                   movel     'C'           t02mod
012200021115     c                   movel     knsif         t02sif
012300021115     c                   movel     'DFE'         t02cod
012400021118     c                   movel(p)  'FED'         t02ke1
012500021115     c                   call      'TIBS02R'
012600021115     c                   parm                    kpjba
012700021115     c                   parm                    tibs02ds
012800021115
012900021115     c                   if        t02err = *blanks
013000021115     c                   movel     t02uni        ddfe
013100021115     c                   endif
013200120920     C*
013300120920     C                   ENDIF
013400020211
013500020211      * Network già passato
013600020213 b1  c                   if        i27ntw <> *blanks
013700020211     c                   select
014300020211      * DPD
014400020211     c                   when      i27ntw = 'DPD'
014500020211     c                   eval      o27ksc = §tcadc
014600020211     c                   eval      o27ctr = §tcadt
014700020218     c                   eval      o27fie = 'D'
014800020211      * EuroExpress
014900020301     c                   when      i27ntw = 'EEX' or
015000020301     c                             i27ntw = 'EUP'
015100020211     c                   eval      o27ksc = §tcaec
015200020211     c                   eval      o27ctr = §tcaet
015300020218     c                   eval      o27fie = 'E'
015400020211      * FedEx
015500020211     c                   when      i27ntw = 'FED'
015900021115     c                   exsr      tarfedex
016000020211      * Bartolini
016100020211     c                   other
016200020211     c                   eval      o27ksc = §tcabc
016300020211     c                   eval      o27ctr = §tcabt
016400020218     c                   eval      o27fie = 'I'
016500020211     c                   endsl
016600020211
016700020213     c                   eval      o27ntw = i27ntw
016800020211     c                   goto      endela
016900020213 e1  c                   endif
017000011106
017900020208
027100020208
027200020208      * Altro
027300020211     c                   eval      o27ksc = §tcabc
027400020211     c                   eval      o27ctr = §tcabt
027500020213     c                   eval      o27ntw = 'COR'
027600020218     c                   eval      o27fie = 'I'
027700011029
027800020211     c     endela        tag
027900020211
028000020211      * Passata la data recupero progressivo
028100020213 b1  c                   if        i27dta <> *zeros
028200020211      * Carico i capoconti se mai fatto
028300020211     c                   if        tcu(1) = *blanks
028400020211     c                   exsr      caricaconti
028500020211     c                   endif
028600020211      * Cerco progressivo
028700020211     c                   clear                   fnlv51ds
028800020215     c                   eval      d51tla = i27tla
028900020211     c                   eval      d51dsp = i27dta
029000020211     c                   eval      d51kci = kci
029100020211     c                   eval      d51ksc = o27ksc
029200020211     c                   movel     o27ctr        d51ctr
029300020211     c                   eval      d51ric = 'S'
029400020218     c                   eval      d51fie = o27fie
029500020211     c                   call      'FNLV51R'
029600020211     c                   parm                    fnlv51ds
029700020319     c                   if        d51err = *blanks AND D51TKS='S'
029800020211     c                   movel     d51prg        o27prg
029900020211     c                   endif
030000020213 e1  c                   endif
030100020211
030200020211     c                   endsr
030300021115      *----------------------------------------------------
030400021115      * Carico tariffa FedEx
030500021115      *----------------------------------------------------
030600021115     c     TarFedEx      BegSr
030700021115
030800021115     c                   Clear                   Wctb
030900021115
031000021115      * Se passato il codice tariffa devo stabilire in quale range
031100021115      * se merce o documenti
031200021115      * controllo solo gli ultimi due caratteri del codice tariffa passato
031300021115If  1c                   If        I27Ctb <> *Blanks
031400021115     c                   Move      I27Ctb        WCtb
031500021115If  2c                   If        WCtb >= §Dfedalm and WCtb <= §Dfealm
031600021115     c                   Eval      O27Ksc = §Tcafc
031700021115     c                   Eval      O27Ctr = §Tcaft
031800021115     c                   Eval      O27Fie = 'M'
031900021115    2c                   EndIf
032000021115If  2c                   If        WCtb >= §Dfedald and WCtb <= §Dfeald
032100021115     c                   Eval      O27Ksc = §Tcafcd
032200021115     c                   Eval      O27Ctr = §Tcaftd
032300021115     c                   Eval      O27Fie = 'N'
032400021115    2c                   EndIf
032500021115     c                   Goto      EndTarFedEx
032600021115    1c                   EndIf
032700021115
032800021115      * Se passato il tipo tariffa
032900021115      * per FedEx FM --> Merce
033000021115      *           FD --> Documenti
033100021115      * prendo la relativa cartello
033200021115If  1c                   If        I27Tip = 'FM'
033300021115     c                   Eval      O27Ksc = §Tcafc
033400021115     c                   Eval      O27Ctr = §Tcaft
033500021115     c                   Eval      O27Fie = 'M'
033600021115     c                   Goto      EndTarFedEx
033700021115    1c                   EndIf
033800021115If  1c                   If        I27Tip = 'FD'
033900021115     c                   Eval      O27Ksc = §Tcafcd
034000021115     c                   Eval      O27Ctr = §Tcaftd
034100021115     c                   Eval      O27Fie = 'N'
034200021115     c                   Goto      EndTarFedEx
034300021115    1c                   EndIf
034400021115
034500021115      * Se passato il peso
034600021115      * testo se superiore al limite previsto per la tariffa documenti
034700021115If  1c                   If        I27Pkg > *Zeros
034800021115If  2c                   If        I27Pkg > §DfePkgd
034900021115     c                   Eval      O27Ksc = §Tcafc
035000021115     c                   Eval      O27Ctr = §Tcaft
035100021115     c                   Eval      O27Fie = 'M'
035200021115   x2c                   Else
035300021115     c                   Eval      O27Ksc = §Tcafcd
035400021115     c                   Eval      O27Ctr = §Tcaftd
035500021115     c                   Eval      O27Fie = 'F'
035600021115    2c                   EndIf
035700021115     c                   Goto      EndTarFedEx
035800021115    1c                   EndIf
035900021115
036000021115      * Se non sono stati passati i campi necessari per reperire la cartello giusta
036100021115      * imposto lo stesso la cartello FEdEx merce, ma segnalo un errore
036200021115     c                   Eval      O27Ksc = §Tcafc
036300021115     c                   Eval      O27Ctr = §Tcaft
036400021115     c                   Eval      O27Fie = 'F'
036500021115      * Imposto anche un msg di errore
036600021115     c                   Eval      O27Err = 'E'
036700021115     c                   Eval      O27Msg = Msg(2)
036800021115
036900021115     c     EndTarFedEx   EndSr
037000011106      *----------------------------------------------------
037100020207      * Carico filiali da azorg
037200011106      *----------------------------------------------------
037300020207     c     caricafil     begsr
037400020207
037500020207     c                   open      azorg01l
037600020207
037700020211     c                   clear                   xx
037800020207
037900020207     c     *loval        setll     azorg01l
038000020207     c                   do        *hival
038100020207     c                   read      azorg01l
038200020207
038300020207     c                   if        %eof
038400020207     c                   leave
038500020207     c                   endif
038600020207
038700020207     c                   if        orgfva <> *blanks
038800020207     c                   iter
038900020207     c                   endif
039000020207
039100020207     c                   if        orgfag <> 'A' and orgfag <> 'F'
039200020207     c                   iter
039300020207     c                   endif
039400020207
039500020207     c                   eval      og143 = orgde3
039600020208
039700020208     c                   if        §ogntw = 'DPD' or
039800020208     c                             §ogntw = 'EEX' or
039900020301     c                             §ogntw = 'FED' or
040000020301     c                             §ogntw = 'EUP'
040100020207     c                   eval      xx = xx + 1
040200020207     c                   eval      fil(xx) = orgfil
040300020207     c                   eval      ntw(xx) = §ogntw
040400020208     c                   endif
040500020207     c                   enddo
040600020207
040700020207     c                   close     azorg01l
040800011106
040900011106     c                   endsr
041000020211      *----------------------------------------------------
041100020211      * Carico capoconti contabili
041200020211      *----------------------------------------------------
041300020211     c     caricaconti   begsr
041400020211
041500020211
041600020211     c                   eval      codut = 1
041700020211     c                   call      'X§PARUT'
041800020211     c                   parm                    ut§dse0f
041900020211
042000020211      * Ricerca Capoconti
042100020211     c                   do        50            x
042200020211     c                   move      tcu(x)        tcuds
042300020211     c     f56           cabne     'CG'          end1
042400020211     c     f4            comp      '1'                                    21
042500020211     c     f4            comp      '2'                                    22
042600020211     c     f4            comp      '3'                                    23
042700020211     c     f4            comp      '6'                                    27
042800020211
042900020211      ** 1 CLIENTI   21
043000020211      ** 2 FORNITORI 22
043100020211      ** 3 AGENTI    23
043200020211     c     f3            comp      '0'                                242425
043300020211     c     f3            comp      'I'                                    26
043400020211
043500020211      ** 0 ITALIA   25
043600020211      ** 1 ESTERO   24
043700020211      ** I CAPO CONTO IVA
043800020211     c   21
043900020211     can 24              z-add     kcu(x)        kce               4 0
044000020211     c   21
044100020211     can 25              z-add     kcu(x)        kci               4 0
044200020211     c   22
044300020211     can 24              z-add     kcu(x)        kfe               4 0
044400020211     c   22
044500020211     can 25              z-add     kcu(x)        kfi               4 0
044600020211     c   23
044700020211     can 24              z-add     kcu(x)        kae               4 0
044800020211     c   23
044900020211     can 25              z-add     kcu(x)        kai               4 0
045000020211     c   26              z-add     kcu(x)        kiva              4 0
045100020211     c   27              z-add     kcu(x)        kbna              4 0
045200020211
045300020211     c     end1          tag
045400020211     c                   enddo
045500020211
045600020211     c                   setoff                                       212223
045700020211     c                   setoff                                       242526
045800020211     c                   setoff                                       27
045900020211
046000020211     c                   endsr
046100020211
046200020207** MSG  Lungh. 78                                                            *
046300020214TRUL27R - Non trovata la tabella TCA, telefonare in SEDE                            1
046400021115TRUL27R - Mancano i dati per reperire la corretta Tar.Cartello FedEx                2
