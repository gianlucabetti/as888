000100040830      *===============================================================*
000200101104      * TIBS39R - Aggiornamento LIBRERIA CORRENTE nei profili utente  *
000300101104      *           da tabella "CUR"                                      *
000400040830      *===============================================================*
000500040831      *? COMPILARE CON PROFILO QSECOFR                               ?*
000600040831      *===============================================================*
000700040830
000800040830     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000900040831     h Usrprf(*owner)
001000040830
001100040830      *===============================================================*
001200040830
001300040830     fAZORG01L  if   e           k disk
001400040830     fAZUTE02L  if   e           k disk
001500040830      *
001600040830      *===============================================================*
001700040830      *
001800040830      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001900040831      * - Comandi da eseguire
002000040831     d $Cmd            s             80    dim(02) ctdata  perrcd(1)
002100040830      *
002200040830      * Strutture Dati - - - - - - - - - - - - - - - - - - - - - - - -*
002300120517     d
002400120517     d tibs40ds      e ds
002500120517     D wfil                    9   1808  0 DIM(600)
002600120517     D wlib                 1809   7808    DIM(600)
002700120420     d
002800120420     D tibs39ds        DS                  INZ
002900120420     D  bs39agg                1      1
003000120420     D  bs39sim                2      2
003100120420     D  bs39email              3     27
003200040830      *
003300040830     d KPJBA         e ds
003400040830      * - Parametri x Controllo profilo utenti
003500040830     d TIBS34DS      e ds                  inz
003600040830      * - Ds di riferimento al file esterno AZUTE00F
003700040830     d AZUTEDS       e ds                  extname(AZUTE00F)
003800040830      * - Ds per dati organigramma
003900040830     d DDatiUte      e ds
004000040830      * - Ds per reperimento tabelle
004100040830     d Tibs02Ds      e ds                  inz
004200120420     d
004300040830     d Status         sds           333
004400040830     d   SDSpgm          *proc
004500040830      *
004600040831      * - Indici di schiera
004700120419     d xx              s              4  0 inz
004800120419     d PP              s              3  0 inz
004900120419     d MM              s              3  0 inz
005000120419     d EE              s              3  0 inz
005100040831      * - Campi per QCMDEXC
005200040831     d Qcmd            s             80    inz
005300040831     d Qlen            s             15  5 inz(80)
005400101104     d
005500120419     d WuteE           s           5000
005600120419     d WuteM           s           5000
005700120419     d WuteP           s           5000
005800120419     d kfil            s              3  0 inz
005900101104     d Datasys         s               d   datfmt(*iso) inz(*sys)
006000120420     d CURLIB          s             10    inz('*CRTDFT   ')
006100120420     d wogg            s             23
006200040830
006300120503     D Ccceml          C                   CONST('ced@brt.it')
006400120503     D Ceml            C                   CONST('elisa.sanfelici@brt.it')
006500120420     D Cagg            C                   CONST('Aggiornamento profili:')
006600120420     D Csim            C                   CONST('SIMULA aggiorn.profili:')
006700120420     d
006800120420     D Coggp           c                   const(' aggiunte GAICUR')
006900120420     D Coggm           c                   const(' eliminate GAICUR')
007000120420     D Cogge           c                   const('aggiornam.falliti')
007100040830      *===============================================================*
007200040830     c     *Entry        plist
007300040830     c                   parm                    KPJBA
007400120419     c* controllo se devo elaborare solo in aggiunta
007500120420     c                   movel     kpjbu         tibs39ds
007600040830      *
007700040830      * Operazioni iniziali
007800120419     c                   exsr      DatiJob
007900040830      *
008000101104     c                   exsr      Elabora
008100040830      * Fine
008200040830     c                   eval      *inLR = *on
008300040830      *
008400101104      *---------------------------------------------------------------*
008500101104      * Leggo la tabella CUR                                          *
008600101104      *---------------------------------------------------------------*
008700101104     c     Elabora       BEGSR
008800120419     c* Leggo tutti i record della tabella CUR e memorizzo le filiali che
008900120419     c*  devono avere il profilo con la GAICUR  con data decorrenza <= udate
009000120419     c                   clear                   wfil
009100120419     c                   clear                   wlib
009200120419     c                   clear                   wutep
009300120419     c                   clear                   wutem
009400120419     c                   clear                   wutee
009500120517     c
009600120517     c* carico filiali e librerie della CUR
009700120517     c                   clear                   tibs40ds
009800120517     c                   call      'TIBS40R'
009900120517     c                   parm                    kpjba
010000120517     c                   parm                    tibs40ds
010100120419     c
010200101104     c
010300120419     c* Caricate tutte le filiali che devono avere la CUR    --> vado ad aggiornare
010400120419     c                   exsr      Exechg
010500120419     c* Invio email di cosa ho fatto
010600120419     c                   clear                   peml            253
010700120503     c                   clear                   pcceml          253
010800120503     c                   clear                   pesito            1
010900120420     c                   clear                   wogg
011000120420     c* indirizzo e-mail
011100120420     c                   if        bs39email=*blanks
011200120419     c                   eval      peml=cEML
011300120503     c                   eval      pcceml=cCcEML
011400120420     c                   else
011500120420     c                   eval      peml=%trim(bs39email)+'@brt.it'
011600120420     c                   endif
011700120420     c* Oggetto
011800120420     c                   if        bs39sim='S'
011900120420     c                   eval      wogg=csim
012000120420     c                   else
012100120420     c                   eval      wogg=cagg
012200120420     c                   endif
012300120419     c
012400120419     c                   if        wutep<>*blanks
012500120420     c                   clear                   pogg             44
012600120419     c                   clear                   pmsg           5000
012700120420     c                   eval      pogg=wogg+coggp
012800120419     c                   movel     wutep         pmsg
012900120503     c                   call      'TIS701C1'
013000120419     c                   parm                    peml
013100120503     c                   parm                    pcceml
013200120419     c                   parm                    pogg
013300120419     c                   parm                    pmsg
013400120503     c                   parm                    pesito
013500120419     c                   endif
013600120419     c
013700120419     c                   if        wutem<>*blanks
013800120420     c                   clear                   pogg             44
013900120419     c                   clear                   pmsg           5000
014000120420     c                   eval      pogg=wogg +coggm
014100120419     c                   movel     wutem         pmsg
014200120503     c                   call      'TIS701C1'
014300120419     c                   parm                    peml
014400120503     c                   parm                    pcceml
014500120419     c                   parm                    pogg
014600120419     c                   parm                    pmsg
014700120503     c                   parm                    pesito
014800120419     c                   endif
014900120419ù    c                   if        wutee<>*blanks
015000120420     c                   clear                   pogg             44
015100120419     c                   clear                   pmsg           5000
015200120420     c                   eval      pogg=wogg +cogge
015300120419     c                   movel     wutee         pmsg
015400120503     c                   call      'TIS701C1'
015500120419     c                   parm                    peml
015600120503     c                   parm                    pcceml
015700120419     c                   parm                    pogg
015800120419     c                   parm                    pmsg
015900120503     c                   parm                    pesito
016000120419     c                   endif
016100120419     c
016200101104     c                   ENDSR
016300040830      *---------------------------------------------------------------*
016400040830      * Reperimento Dati del job (Utente/Operativi)                   *
016500040830      *---------------------------------------------------------------*
016600040830     c     DatiJob       BEGSR
016700040830      *
016800040830     c     *dtaara       define    §azute        azuteds
016900040830     c     *dtaara       define    §datiute      ddatiute
017000040830      *
017100040830     c                   in(E)     *dtaara
017200040830     c                   IF        %ERROR or RSUT = *blanks
017300040830     c                   clear                   Tibs34Ds
017400040830     c                   call      'TIBS34R'
017500040830     c                   parm                    Tibs34Ds
017600040830     c                   in        *dtaara
017700040830     c                   ENDIF
017800101104      *
017900120419     c                   movel     datasys       dataoggi          8 0
018000120516     c* Se festivo, cerco il primo giorno lavorativo superiore
018100040830     c                   ENDSR
018200040830      *
018300101104      *---------------------------------------------------------------------*
018400101104      * Esecuzione del comando CHGUSRPRF per ogni profilo che cambia terminal
018500101104      *---------------------------------------------------------------------*
018600040830     c     ExeCHG        BEGSR
018700120419     c                   eval      pp=1
018800120419     c                   eval      ee=1
018900120419     c                   eval      mm=1
019000120419     c
019100120426      * Ciclo di lettura file AZUTE solo in aggiunta
019200120420    1c                   if        bs39agg  ='S'
019300120419     c
019400120419     c                   z-add     1             xx
019500120419    2c                   dow       wfil(xx)>0
019600120419     c                   eval      kfil=wfil(XX)
019700120419     c
019800120419     c     kfil          setll     AZUTE000
019900120419     c     kfil          reade     AZUTE000
020000120419      *
020100120419do  3c                   DOW       NOT %eof(AZUTE02L)
020200150921     c* Escludo i profili della 046
020300170315   3ac                   if        utefil<>046 and uteute<>'EDPCEDXXX'
020400120419     c                   clear                   w32curl
020500120419     c                   clear                   w32err
020600150914     c                   clear                   w39piena          1
020700120419     c                   exsr      callTIBS32
020800120419
020900120419    4c                   if        w32curl<>wlib(xx)
021000120426     c
021100120426     c* se c'e' un'altra gaicur non eseguo aggiornamento
021200120426     c
021300120426    5c                   if        (%subst(w32curl:1:1)= ' ' or
021400120426     c                                             %subst(w32curl:1:1) = '*')
021500120426     c
021600120419      *
021700120419      * - Aggiornamento:
021800120419      *   imposto libreria corrente (CURLIB)
021900120419     c                   eval      Qcmd = %trim($Cmd(1))
022000120419     c                                  + %trim(UTEute)    + ') '
022100120419     c                                  + %trim($Cmd(2))
022200120419     c                                  + %trim(wlib(xx)) + ')'
022300120420     c
022400120420     c* Eseguo solo se non sono in simulazione
022500120420     c                   if        bs39sim<>'S'
022600120420     c                   call      'QCMDEXC'                            90
022700120419     c                   parm                    Qcmd
022800120419     c                   parm                    Qlen
022900120420     c                   endif
023000120419
023100120419     c* Aggiornamento non riuscito
023200120426    6c                   if        *in90
023300120419     c                   eval      wuteE=%trim(wuteE)+' '+
023400120420     c                                       %trim(uteute)+ '/Cur'+
023500120419     c                                       %subst(wlib(xx):7:3)
023600120419     c                   else
023700120419     c                   eval      wuteP=%trim(wuteP)+' '+
023800120420     c                                       %trim(uteute)+ '/Cur'+
023900120419     c                                       %subst(wlib(xx):7:3)
024000120419     c                   eval      pp=pp+1
024100120426    6c                   endif
024200120426     c
024300120426   x4c                   else
024400120426     c* Errore di modifica gaicur non effettuata
024500120426     c                   eval      wuteE=%trim(wuteE)+' '+
024600120426     c                                   %trim(uteute)+ '/Cur'+
024700120426     c                                   %subst(w32curl:7:3)
024800120426     c                   eval      ee=ee+1
024900120426    5c                   endif
025000120426    4c                   endif
025100150921   3ac                   endif
025200120419      *
025300120419     c     kfil          reade     AZUTE000
025400120419      *
025500120419e   3c                   ENDDO
025600120419     c
025700120419     c                   add       1             xx
025800120419e   2c                   ENDDO
025900120419     c
026000120419   x1c                   else
026100040830      *
026200120419      * Ciclo di lettura file AZUTE tutto (aggiunta ed eliminazione)
026300120419     c                   clear                   kfil
026400120419     c     *loval        setll     AZUTE000
026500120419     c                   read      AZUTE000
026600040830      *
026700040901do  2c                   DOW       NOT %eof(AZUTE02L)
026800170315   2ac                   if        utefil<>046 and uteute<>'EDPCEDXXX'
026900120419     c
027000120419     c* A cambio filiale controllo se da aggiungere o eliminare(se era presente ma senza dati)
027100120419    3c                   if        utefil<>kfil
027200120419     c                   clear                   libute           10
027300120419     c                   z-add     0             xx
027400120419     c                   eval      xx=%lookup(utefil:wfil)
027500120419    4c                   if        xx>0
027600120419     c                   eval      libute  =wlib(xx)
027700120419     c                   else
027800120420     c                   eval      libute  =curlib
027900120419    4c                   endif
028000120419     c
028100120419     c                   eval      kfil=utefil
028200120419    3c                   endif
028300120419     c
028400120419     c* se non devo impostare la gaicur --> verifico se c'è e se posso toglierla
028500120419     c                   clear                   w32curl
028600120419     c                   clear                   w32err
028700120419     c                   clear                   w39piena
028800120419     c
028900120419     c                   exsr      Calltibs32
029000120419     c
029100120426     c* Se trovo una gaicur EDP --> non tocco il profilo
029200150914   3ac                   if        %subst(w32curl:1:3)<>'EDP'
029300120426     c
029400150914    4c**                 if        w32err=' '  and %subst(w32curl:1:1)<>' '  and
029500150914     c**                                           %subst(w32curl:1:1)<>'*'
029600120419     c
029700120419     c* se la gaicur contiene degli oggetti--> non posso toglierla
029800150914     c**                 call      'TIBS39C'
029900150914     c**                 parm      w32curl       w39lib           10
030000150914     c**                 parm      'N'           w39piena          1
030100150914    4c**                 endif
030200120419     c
030300150914     c* 14/09/15: su indicazione di Luciano, tolgo la GAICURxxx anche se piena
030400150914     c*           dai profili. Eliminato quindi richiamo al TIBS39C che effettua il controllo
030500150914     c                   eval      w39piena='N'
030600150914     c
030700120420     c* Aggiorno solo in aggiunta se manca
030800120426     c* updato   solo                 se gaicur c'e' e vuota da oggetti in prova
030900120426     c* Aggiorno solo in eliminazione se gaicur c'e' e vuota da oggetti in prova
031000120426    3c                   if        (libute<>curlib and w32curl<>libute and
031100120426     c                                     w39piena<>'S')  or
031200120426     c                             (libute= curlib and
031300120426     c                              %subst(w32curl:1:1)<> ' '    and
031400120426     c                              %subst(w32curl:1:1)<>'*'     and
031500120419     c                                     w39piena='N')
031600040830      *
031700040831      * - Aggiornamento:
031800120420      *   imposto libreria corrente
031900040831     c                   eval      Qcmd = %trim($Cmd(1))
032000040831     c                                  + %trim(UTEute)    + ') '
032100040831     c                                  + %trim($Cmd(2))
032200120419     c                                  + %trim(libute) + ')'
032300120420     c* Eseguo solo se non sono in simulazione
032400120420     c                   if        bs39sim<>'S'
032500120420     c                   call      'QCMDEXC'                            90
032600040831     c                   parm                    Qcmd
032700040831     c                   parm                    Qlen
032800120420     c                   endif
032900120419
033000120419    4c                   if        *in90
033100120419     c                   eval      wuteE=%trim(wuteE)+' '+
033200120420     c                                       %trim(uteute)+ '/Cur'+
033300120419     c                                        %subst(libute:7:3)
033400120419     c                   eval      ee=ee+1
033500120419   x4c                   else
033600120419     c
033700120420    5c                   if        libute<>curlib
033800120419     c                   eval      wuteP=%trim(wuteP)+' '+
033900120420     c                                       %trim(uteute)+ '/Cur'+
034000120419     c                                        %subst(libute:7:3)
034100120419     c                   eval      pp=pp+1
034200120419     c
034300120419   x5c                   else
034400120419     c                   eval      wuteM=%trim(wuteM)+' '+
034500120420     c                                       %trim(uteute)+'/Cur'+
034600120419     c                                        %subst(w32curl:7:3)
034700120419     c                   eval      mm=mm+1
034800120419    5c                   endif
034900120419    4c                   endif
035000040830      *
035100120419   x3c                   else
035200120419     c*
035300120426    4c                   if        w39piena='S' and w32curl<>libute
035400120419     c                   eval      wuteE=%trim(wuteE)+' '+
035500120426     c                                   %trim(uteute)+ '/Cur'+
035600120426     c                                    %subst(w32curl:7:3)+'Full'
035700120419     c                   eval      ee=ee+1
035800120419    4c                   endif
035900120426   3ac                   endif
036000120426    3c                   endif
036100150921   2ac                   endif
036200120419     c
036300120420     c                   read      AZUTE000
036400040830      *
036500040901e   2c                   ENDDO
036600120419    1c                   endif
036700040901      *
036800040901     c                   ENDSR
036900120419      *---------------------------------------------------------------------*
037000120419     c     CallTIBS32    BEGSR
037100120419     c                   call      'TIBS32C'
037200120419     c                   parm      'CHK'         w32Ctela          3
037300120419     c                   parm      *blanks       w32Cmoba          1
037400120419     c                   parm      UTEute        w32Cuser         10
037500120419     c                   parm      *blanks       w32Ctext         50
037600120419     c                   parm      *blanks       w32curl          10
037700120419     c                   parm      *blanks       w32Cattn         10
037800120419     c                   parm      *blanks       w32Clatt         10
037900120419     c                   parm      *blanks       w32Cscad          4
038000120419     c                   parm      *blanks       w32Cpwd          10
038100120419     c                   parm      *blanks       w32Csts           9
038200120419     c                   parm      *blanks       w32err            1
038300120419     c                   ENDSR
038400040830
038500040831** $Cmd
038600040831CHGUSRPRF  usrprf(
038700040831           curlib(
