000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200011011      *
000300160211      *  Recupero network bolla per caricamento tariffe
000400160211      * -----------------------------------------------
000500020207
000600020207     fazorg01l  if   e           k disk    usropn
000700011019
000800020619     d fil             s              3  0 dim(50)                              filiale
000900020619     d ntw             s              3    dim(50)                              Network
001000021115     d msg             s             78    dim(10) ctdata perrcd(1)
001100020207
001200160211     d trul27ds1     e ds
001300020208     d og143         e ds
001400021115     d ddfe          e ds
001500011029     d tibs02DS      e ds
001600011030     d kpjba         e ds
001700020211
001800020211     d x               s              2  0
001900020211     d xx              s              3  0
002000020208     d yy              s              3  0
002100020208     d pocli           s              3  0
002200020214     d wi27tln         s                   like(i27tln)
002300021115     d Wctb            s              2  0
002400120920     D Volta1          s               n
002500011019
002600011019      *----------------------------------------------------------------
002700011019
002800011029     c     *entry        plist
002900160211     c                   parm                    trul27ds1
003000020214
003100020214      * non posso avere i27lna e i27tln valorizzati
003200020214      * la linea di arrivo vince sul tipo linea di arrivo
003300020214     c                   eval      wi27tln = i27tln
003400020214     c                   if        wi27tln <> *blanks and i27lna <> *zeros
003500020214     c                   clear                   wi27tln
003600020214     c                   endif
003700011019
003800020220     c                   eval      *in30 = *off
003900020220     c                   eval      *in31 = *off
004000020213     c                   clear                   o27ntw
004100020218     c                   clear                   o27fie
004200020211     c                   clear                   o27err
004300020211     c                   clear                   o27msg
004400011031
004500011031      * Controllo validità tipo lancio
004600020211     c                   if        i27tla <> 'C' and i27tla <> 'L' and
004700020211     c                             i27tla <> *blanks
004800020211     c                   clear                   i27tla
004900011031     c                   endif
005000011019
005100011019      * Se Tipo lancio 'C' --> solo chiusura file
005200020211     c                   if        i27tla <> 'C'
005300011029     c                   exsr      elabora
005400011029     c                   endif
005500011011
005600020211     c                   if        i27tla <> *blanks
005700011031     c                   eval      *inlr = *on
005800011029     c                   else
005900011031     c                   eval      *inrt = *on
006000011029     c                   endif
006100011011
006200011011      *----------------------------------------------------
006300011019      * ELABORAZIONE
006400011011      *----------------------------------------------------
006500011029     c     elabora       begsr
006600020208
006700120920     C* eseguo questo reperimento solo la prima volta
006800120920     C                   IF        Volta1 <> *on
006900120920     C                   EVAL      Volta1 =  *on
007000021115
007100021115      * aggancio la tabella DFE
007200021115     c                   clear                   ddfe
007300021115     c                   clear                   tibs02ds
007400150821     c                   movel     'L'           T02tla
007500021115     c                   movel     'C'           t02mod
007600021115     c                   movel     knsif         t02sif
007700021115     c                   movel     'DFE'         t02cod
007800021118     c                   movel(p)  'FED'         t02ke1
007900021115     c                   call      'TIBS02R'
008000021115     c                   parm                    kpjba
008100021115     c                   parm                    tibs02ds
008200021115
008300021115     c                   if        t02err = *blanks
008400021115     c                   movel     t02uni        ddfe
008500021115     c                   endif
008600120920     C*
008700120920     C                   ENDIF
008800160226     c* Verifico, se passato codice cliente, se è un non codoficato
008900160226     c                   move      I27cli        wfine             4 0
009000160211     c
009100020211      * Network già passato
009200020213 b1  c                   if        i27ntw <> *blanks
009300020211     c                   select
009400020211      * DPD
009500020211     c                   when      i27ntw = 'DPD'
009600160226
009700160226     c                   select
009800160226     c                   when      wfine=8888 or wfine=9999
009900160226     c                   eval      o27fie = 'D'
010000160226     c
010100160226     c                   when      §attx='S'
010200160211     c                   eval      o27fie = 'X'
010300160226     c                   other
010400160222     c                   eval      o27fie = 'D'
010500160226     c                   endsl
010600020211      * EuroExpress
010700160211     c                   when      i27ntw = 'EEX'
010800160226     c                   select
010900160226     c                   when      wfine=8888 or wfine=9999
011000160226     c                   eval      o27fie = 'E'
011100160226     c
011200160226     c                   when      §attx='S'
011300160211     c                   eval      o27fie = 'X'
011400160226     c                   other
011500160222     c                   eval      o27fie = 'E'
011600160226     c                   endsl
011700020211      * FedEx
011800020211     c                   when      i27ntw = 'FED'
011900021115     c                   exsr      tarfedex
012000020211     c                   other
012100160211      * BRT
012200020218     c                   eval      o27fie = 'I'
012300020211     c                   endsl
012400020211
012500020213     c                   eval      o27ntw = i27ntw
012600020211     c                   goto      endela
012700020213 e1  c                   endif
012800020208
012900020207      * Carico dati azorg in schiera se mai fatto
013000020207     c                   if        fil(1) = *zeros
013100020207     c                   exsr      caricafil
013200020207     c                   endif
013300020208
013400020213      * filiale + network per lna o tipo linea di arrivo
013500020214 b1  c                   if        i27lna <> *zeros or wi27tln <> *blanks
013600020213      * controllo se lna caricata nella sk
013700020213     c                   if        i27lna <> *zeros
013800020211     c                   eval      xx = 1
013900020211     c     i27lna        lookup    fil(xx)                                30
014000020213     c                   endif
014100020208
014200020208      * Linea di arrivo DPD
014300020214     c                   if        (*in30 and ntw(xx) = 'DPD') or wi27tln = 'D'
014400020213     c                   eval      o27ntw = 'DPD'
014500160226
014600160226     c                   select
014700160226     c                   when      wfine=8888 or wfine=9999
014800160226     c                   eval      o27fie = 'D'
014900160226     c
015000160226     c                   when      §attx='S'
015100160211     c                   eval      o27fie = 'X'
015200160226     c                   other
015300160222     c                   eval      o27fie = 'D'
015400160226     c                   endsl
015500020208     c                   goto      endela
015600020208     c                   endif
015700020208      * Linea di arrivo EuroExpress
015800160211     c                   if        (*in30 and  ntw(xx) = 'EEX' )
015900020301     c                              or wi27tln = 'E'
016000020213     c                   eval      o27ntw = 'EEX'
016100160226
016200160226     c                   select
016300160226     c                   when      wfine=8888 or wfine=9999
016400160226     c                   eval      o27fie = 'E'
016500160226     c
016600160226     c                   when      §attx='S'
016700160211     c                   eval      o27fie = 'X'
016800160226     c                   other
016900160222     c                   eval      o27fie = 'E'
017000160226     c                   endsl
017100020208     c                   goto      endela
017200020208     c                   endif
017300020208      * Linea di arrivo FedEx
017400020214     c                   if        (*in30 and ntw(xx) = 'FED') or wi27tln = 'F'
017500020213     c                   eval      o27ntw = 'FED'
017600021115     c                   exsr      tarfedex
017700020208     c                   goto      endela
017800020208     c                   endif
017900020208
018000020213 b1  c                   endif
018100020208
018200020208      * filiale + network per po cliente o lnp
018300020208     c                   eval      xx = 1
018400020208     c                   eval      yy = 1
018500020211     c                   if        i27cli <> *zeros
018600020211     c                   movel     i27cli        pocli
018700020208     c     pocli         lookup    fil(xx)                                30
018800020208     c                   endif
018900020211     c                   if        i27lnp <> *zeros
019000020211     c     i27lnp        lookup    fil(yy)                                31
019100020208     c                   endif
019200020208
019300020208      * P.O. cliente o linea di partenza DPD
019400020208     c                   if        (*in30 and ntw(xx) = 'DPD') or
019500020208     c                             (*in31 and ntw(yy) = 'DPD')
019600020213     c                   eval      o27ntw = 'DPD'
019700160226
019800160226     c                   select
019900160226     c                   when      wfine=8888 or wfine=9999
020000160226     c                   eval      o27fie = 'D'
020100160226     c                   when      §attx='S'
020200160211     c                   eval      o27fie = 'X'
020300160226     c                   other
020400160222     c                   eval      o27fie = 'D'
020500160226     c                   endsl
020600020208     c                   goto      endela
020700160226     c                   endif
020800160226
020900020208      * P.O. cliente o linea di partenza EuroExpress
021000160211     c                   if        (*in30 and  ntw(xx) = 'EEX') or
021100160211     c                             (*in31 and  ntw(yy) = 'EEX')
021200020213     c                   eval      o27ntw = 'EEX'
021300160226
021400160226     c                   select
021500160226     c                   when      wfine=8888 or wfine=9999
021600160226     c                   eval      o27fie = 'D'
021700160226     c                   when      §attx='S'
021800160211     c                   eval      o27fie = 'X'
021900160226     c                   other
022000160222     c                   eval      o27fie = 'E'
022100160226     c                   endsl
022200020208     c                   goto      endela
022300020208     c                   endif
022400020208      * P.O. cliente o linea di partenza FedEx
022500020208     c                   if        (*in30 and ntw(xx) = 'FED') or
022600020208     c                             (*in31 and ntw(yy) = 'FED')
022700020213     c                   eval      o27ntw = 'FED'
022800021115     c                   exsr      tarfedex
022900020208     c                   goto      endela
023000020208     c                   endif
023100020208
023200020208      * Altro
023300020213     c                   eval      o27ntw = 'COR'
023400020218     c                   eval      o27fie = 'I'
023500011029
023600020211     c     endela        tag
023700020211
023800020211     c                   endsr
023900021115      *----------------------------------------------------
024000021115      * Carico tariffa FedEx
024100021115      *----------------------------------------------------
024200021115     c     TarFedEx      BegSr
024300021115
024400021115     c                   Clear                   Wctb
024500021115
024600021115      * Se passato il codice tariffa devo stabilire in quale range
024700021115      * se merce o documenti
024800021115      * controllo solo gli ultimi due caratteri del codice tariffa passato
024900021115If  1c                   If        I27Ctb <> *Blanks
025000021115     c                   Move      I27Ctb        WCtb
025100021115If  2c                   If        WCtb >= §Dfedalm and WCtb <= §Dfealm
025200021115     c                   Eval      O27Fie = 'M'
025300021115    2c                   EndIf
025400021115If  2c                   If        WCtb >= §Dfedald and WCtb <= §Dfeald
025500021115     c                   Eval      O27Fie = 'N'
025600021115    2c                   EndIf
025700021115     c                   Goto      EndTarFedEx
025800021115    1c                   EndIf
025900021115
026000021115      * Se passato il tipo tariffa
026100021115      * per FedEx FM --> Merce
026200021115      *           FD --> Documenti
026300021115      * prendo la relativa cartello
026400021115If  1c                   If        I27Tip = 'FM'
026500021115     c                   Eval      O27Fie = 'M'
026600021115     c                   Goto      EndTarFedEx
026700021115    1c                   EndIf
026800021115If  1c                   If        I27Tip = 'FD'
026900021115     c                   Eval      O27Fie = 'N'
027000021115     c                   Goto      EndTarFedEx
027100021115    1c                   EndIf
027200021115
027300021115      * Se passato il peso
027400021115      * testo se superiore al limite previsto per la tariffa documenti
027500021115If  1c                   If        I27Pkg > *Zeros
027600021115If  2c                   If        I27Pkg > §DfePkgd
027700021115     c                   Eval      O27Fie = 'M'
027800021115   x2c                   Else
027900021115     c                   Eval      O27Fie = 'F'
028000021115    2c                   EndIf
028100021115     c                   Goto      EndTarFedEx
028200021115    1c                   EndIf
028300021115
028400021115      * Se non sono stati passati i campi necessari per reperire la cartello giusta
028500021115      * imposto lo stesso la cartello FEdEx merce, ma segnalo un errore
028600021115     c                   Eval      O27Fie = 'F'
028700021115      * Imposto anche un msg di errore
028800021115     c                   Eval      O27Err = 'E'
028900021115     c                   Eval      O27Msg = Msg(2)
029000021115
029100021115     c     EndTarFedEx   EndSr
029200011106      *----------------------------------------------------
029300020207      * Carico filiali da azorg
029400011106      *----------------------------------------------------
029500020207     c     caricafil     begsr
029600020207
029700020207     c                   open      azorg01l
029800020207
029900020211     c                   clear                   xx
030000020207
030100020207     c     *loval        setll     azorg01l
030200020207     c                   do        *hival
030300020207     c                   read      azorg01l
030400020207
030500020207     c                   if        %eof
030600020207     c                   leave
030700020207     c                   endif
030800020207
030900020207     c                   if        orgfva <> *blanks
031000020207     c                   iter
031100020207     c                   endif
031200020207
031300020207     c                   if        orgfag <> 'A' and orgfag <> 'F'
031400020207     c                   iter
031500020207     c                   endif
031600020207
031700020207     c                   eval      og143 = orgde3
031800020208
031900020208     c                   if        §ogntw = 'DPD' or
032000020208     c                             §ogntw = 'EEX' or
032100160211     c                             §ogntw = 'FED'
032200020207     c                   eval      xx = xx + 1
032300020207     c                   eval      fil(xx) = orgfil
032400020207     c                   eval      ntw(xx) = §ogntw
032500020208     c                   endif
032600020207     c                   enddo
032700020207
032800020207     c                   close     azorg01l
032900011106
033000020211     c                   endsr
033100020211
033200020207** MSG  Lungh. 78                                                            *
033300021115TRUL27R - Mancano i dati per reperire la corretta Tar.Cartello FedEx                2
