000100990908     H dftactgrp(*yes)
000200000313     F*
000300040112     Ftabel00f  if   e           k disk
000400000724     Fazorg01l  if   e           k disk
000500040109     Ftitas32c  if   e           k disk
000600050310     Ftigcp51l  if   e           k disk
000700040112     Ffnevb01l  if   e           k disk
000800040112     Ftivin00r  uF   E             disk    usropn
000900040112     FTIVAWWWT  O    E             disk    usropn
001000040112     Ftivlt00f  o    e             disk
001100040112     Ftis7prgf  uf   e             disk    RENAME(tis7prgf:tis7prg0)
001200040112     F                                     PREFIX(f_)
001300000724     D*------------
001400040109     D* DS ESTERNE
001500030811     D*------------
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800000313     D*
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000040112     D ds2a          e ds
002100040112     D*------------
002200040112     D* SCHIERE DI WRK
002300040112     D*------------
002400080903     D sk2Alav         s              3    dim(200)
002500080903     D sk2AlavD        s             75    dim(200)
002600080903     D sk2Agiac        s              3    dim(200)
002700080903     D sk2AgiacD       s             75    dim(200)
002800040109     D*------------
002900040109     D* VARIABILI DI WRK
003000040109     D*------------
003100990910     D esito           s              1
003200000724     D prmlit          s             10
003300000710     D prmfir          s             10
003400990921     D wrkesito        s                   like(esito)
003500040109     D FlgErr          s              1    inz('0')
003600040109     D PosRMN          s              3  0 inz
003700040109     D recCount        s             10  0 inz
003800040109     D savDatRit       s                   like(tasDRT) inz
003900040112     D wMBT            s             10    inz
004000040112     D wPGT            s             10    inz
004100040112     D wPPT            s             10    inz
004200040205     D wPPTKSC         s              7    inz(*all'0')
004300040112     D dwlprg          s             10    INZ(*all'0')
004400040112     D wrkprg          s              8  0 INZ(*zeros)
004500040210     D wvawDTA         s                   LIKE(vawDTA)
004600040209     D*------------------
004700040209     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800040209     D*------------------
004900040209     D WLBDA8          DS                  INZ
005000040209     D  G08DAT                 1      8  0
005100040209     D  G08INV                 9     16  0
005200040209     D  G08ERR                17     17
005300040209     D  G08TGI                18     22  0
005400010606
005500010606
005600010606
005700990921     C                   reset                   esito
005800990921     C                   reset                   wrkesito
005900010601     C*
006000010601     C                   exsr      opeini
006100040109     C                   exsr      runelab
006200040112     C                   exsr      wrivlt
006300010601     C*
006400010601     C                   seton                                        lr
006500000613
006600010601
006700010601
006800010601
006900010601
007000910830     C*--------------------------------------------------------
007100040112     C* RUNELAB LEGGE tivin00r E SCRIVE TIVAWWWT              *
007200910830     C*--------------------------------------------------------
007300040109     C     RUNELAB       BEGSR
007400990910     C*
007500040109     C* FONDAMENTALE!!! Qui imposto quale sia, in sequenza, il campo da considerare come RMN
007600040205 xxx C                   eval      PosRMN = 4
007700040109     C*
007800990914     C                   if        not %open(tivin00r)
007900990908     C                   open      tivin00r
008000990914     C                   endif
008100040109     C                   if        not %open(tivawwwt)
008200040109     C                   open      tivawwwt
008300990914     C                   endif
008400990910     C*
008500040109     C* Leggo sequenzialmente tutto il file
008600040109     C                   do        *HIVAL
008700040109     C                   read      tivin00r                               70
008800040112     C*
008900040112     C* Inizializzo le variabili d appoggio x sigolo riferimento cliente
009000040112     C                   eval      savDatRit = *zeros
009100010618     C*
009200040112     C* Se il record è valorizzato lo elaboro altrimenti nn lo considero e lo considero come ok
009300000905     C                   if        vindta > *blanks
009400990913     C*
009500010601     C                   if        *in70 = *off and
009600990913     C                             (vinflg = *blanks
009700010711     C                              or vinflg = '0'
009800010711     C                              or vinflg = '2')
009900040112     C*
010000040112     C* Incremento il contatore dei record elaborati
010100040112     C                   eval      recCount = recCount + 1
010200990913     C*
010300040109     C* Inizializzo il formato del file d output
010400040109     C                   clear                   TIVAW000
010500040109     C*
010600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010700010711     C                   if        vinflg = *blanks or vinflg = '0'
010800010711     C                   clear                   vinmsg
010900010711     C                   endif
011000010711     C*
011100040109     C* Escludo dall'elaborazione il primo record in quanto contiene l'intestazione dei campi...
011200040109     C* ...effettuo cmq la scritture del file d output x restituire l'intestazione delle colonne
011300040109     C                   if        recCount > 1
011400040109     C*
011500040109     C* Inizializzo l'indicatore d errore che vincola le scritture sul file d output
011600040109     C                   setoff                                       31
011700040109     C*
011800040109     C* Eseguo lo split x estrapolare l'RMN con il quale reperire la bolla relativa
011900040109     C                   movel     *blanks       wRMN             15
012000040109     C                   z-add     *zeros        wCntSep           3 0
012100040109     C                   z-add     *zeros        wPosSepIniz       4 0
012200040109     C                   z-add     *zeros        wPosSepCorr       4 0
012300040109     C                   dow       wPosSepCorr <= %len(%trim(vindta))
012400040109     C                   eval      wPosSepCorr = %scan(';':vindta:wPosSepCorr+1)
012500040109     C                   if        wPosSepCorr > *zeros
012600040109     C                   eval      wCntSep = wCntSep + 1
012700040109     C                   if        wCntSep = PosRMN - 1
012800040109     C                   eval      wPosSepIniz = wPosSepCorr
012900040109     C                   endif
013000040109     C                   if        wCntSep = PosRMN
013100040109     C                   eval      wRMN =
013200040113     C                             %subst(vindta:wPosSepIniz+1:
013300040113     C                             wPosSepCorr-wPosSepIniz-1)
013400040113     C                   leave
013500040109     C                   endif
013600040109     C                   else
013700040109     C                   seton                                        31
013800040109     C                   leave
013900040109     C                   endif
014000040109     C                   enddo
014100040109     C*
014200040109     C* Se estrapoloazione RMN ok => proseguo con l'elaborazione
014300040109     C                   if        *in31 = *off
014400040113     C*
014500040113     C* Trasformo in numerico il valore del RMN così reperito
014600040113     C                   eval      PiStr=wRMN
014700040113     C                   exsr      CHKNUM
014800040113     C                   if        PiNum=*on
014900040113     C                   z-add     PiVal         tasRMN
015000040205     C*
015100040205     C* Verifico se richiesto nei parametri un particolare codice cliente
015200040205     C                   if        wPPTKSC <> *zeros
015300040205     C                   move      wPPTKSC       tasCCM
015400040205     C                   else
015500040113     C                   move      vlrksc        tasCCM
015600040205     C                   endif
015700040109     C     KeyTAS32C     setll     titas32c
015800040109     C                   if        %equal(titas32c)
015900040109     C     KeyTAS32C     reade     titas32c
016000040109     C                   dow       not %eof(titas32c)
016100040112     C*
016200040112     C* A parità d RMN considero la bolla avente data ritiro + recente
016300040112     C                   if        tasDRT > savDatRit
016400040112     C                   eval      savDatRit = tasDRT
016500040209     C*
016600040209     C* Verifico che la bolla NN abbia data consegna richiesta > data odierna
016700040209     C                   if        tasDCR > datcor
016800040209     C                   eval      vawDTA = %trim(vindta) +
016900040209     C                             ';CONSEGNA RICHIESTA;' +
017000040329     C                             %editw(tasDCR:'    /  /  ')+
017100040329     C                             ';'+%editw(tasLNA:'   ')
017200040209     C                   else
017300040112     C*
017400040112     C* Innanzitutto NN considero i DIROTTAMENTI in quanto x medesimo RMN troverò successivamente
017500040112     C* la relativa bolla "figlia"
017600040112     C                   if        tasCCA = '1'
017700040112     C                   else
017800040112     C*
017900040112     C* Eseguo considerazioni x attribuire 1 detrminato STATO alla bolla...
018000040112     C* ...RESO
018100040112     C                   if        tasDCM > *zeros AND tasCCA = '2'
018200040112     C                   eval      vawDTA = %trim(vindta) +
018300040329     C                             ';RESO;'+%editw(tasDCM:'    /  /  ')+
018400040329     C                             ';'+%editw(tasLNA:'   ')
018500040112     C                   else
018600040112     C* ...CONSEGNATA OK
018700040112     C                   if        tasDCM > *zeros AND tasCCA = *blanks
018800040112     C                   eval      vawDTA = %trim(vindta) +
018900040329     C                             ';CONSEGNATA;'+%editw(tasDCM:'    /  /  ')+
019000040329     C                             ';'+%editw(tasLNA:'   ')
019100040112     C                   else
019200040325     C* ...EFFETTIVA APERTURA GIACENZA
019300040325     C                   eval      gcpfrg = *zeros
019400050310     C     KeyGCP51C     chain     tigcp51l
019500050310     C                   if        %found(tigcp51l)
019600040325     C                   movel     '1'           wFlgGiac          1
019700040325     C                   else
019800040325     C                   movel     '0'           wFlgGiac
019900040325     C                   endif
020000040210     C* ...LASCIATO AVVISO/GIACENZE (EVENTI)
020100040112     C     KeyEVB01P     setgt     fnevb01l
020200040112     C     KeyEVB01P     readpe    fnevb01l
020300040209     C                   if        not %eof(fnevb01l)
020400040210     C***                dow       not %eof(fnevb01l)
020500040209     C*
020600040210     C                   z-add     1             i
020700040210     C     evbCEV        lookup    sk2Agiac(i)                            20
020800040210     C                   if        *in20 = *on
020900040325     C*
021000040325     C                   if        wFlgGiac = '0'
021100040210     C                   eval      vawDTA = %trim(vindta) +
021200040210     C                             ';GIACENZA-'+%trim(sk2AgiacD(i))+';' +
021300040329     C                             %editw(evbDEV:'    /  /  ')+
021400040329     C                             ';'+%editw(tasLNA:'   ')
021500040325     C                   else
021600040325     C                   eval      vawDTA = %trim(vindta) +
021700040325     C                             ';GIACENZA-'+%trim(sk2AgiacD(i))     +
021800040325     C                             ' (APERTA GIACENZA)'+';'             +
021900040329     C                             %editw(evbDEV:'    /  /  ')+
022000040329     C                             ';'+%editw(tasLNA:'   ')
022100040325     C                   endif
022200040325     C*
022300040210     C                   else
022400040210     C                   z-add     1             i
022500040210     C     evbCEV        lookup    sk2Alav(i)                             20
022600040112     C                   if        *in20 = *on
022700040112     C                   eval      vawDTA = %trim(vindta) +
022800040210     C                             ';LASCIATO AVVISO-'+%trim(sk2AlavD(i))+';' +
022900040329     C                             %editw(evbDEV:'    /  /  ')+
023000040329     C                             ';'+%editw(tasLNA:'   ')
023100040112     C                   else
023200040112     C                   eval      vawDTA = %trim(vindta) +
023300040209     C                             ';DA CONSEGNARE;'      +
023400040329     C                             %editw(evbDEV:'    /  /  ')+
023500040329     C                             ';'+%editw(tasLNA:'   ')
023600040112     C                   endif
023700040210     C                   endif
023800040210     C                   if        evbCEV = 'MIC'
023900040210     C                   eval      wvawDTA = vawDTA
024000040210     C     KeyEVB01P     readpe    fnevb01l
024100040210     C                   if        not %eof(fnevb01l)
024200040210     C                   if        evbCEV <> 'MIC'
024300040210     C                   z-add     1             i
024400040210     C     evbCEV        lookup    sk2Agiac(i)                            20
024500040210     C                   if        *in20 = *on
024600040325     C*
024700040325     C                   if        wFlgGiac = '0'
024800040210     C                   eval      vawDTA = %trim(vindta) +
024900040210     C                             ';GIACENZA-'+%trim(sk2AgiacD(i))+';' +
025000040329     C                             %editw(evbDEV:'    /  /  ')+
025100040329     C                             ';'+%editw(tasLNA:'   ')
025200040325     C                   else
025300040325     C                   eval      vawDTA = %trim(vindta) +
025400040325     C                             ';GIACENZA-'+%trim(sk2AgiacD(i))     +
025500040325     C                             ' (APERTA GIACENZA)'+';'             +
025600040329     C                             %editw(evbDEV:'    /  /  ')+
025700040329     C                             ';'+%editw(tasLNA:'   ')
025800040325     C                   endif
025900040325     C*
026000040210     C                   else
026100040210     C                   z-add     1             i
026200040210     C     evbCEV        lookup    sk2Alav(i)                             20
026300040210     C                   if        *in20 = *on
026400040210     C                   eval      vawDTA = %trim(vindta) +
026500040210     C                             ';LASCIATO AVVISO-'+%trim(sk2AlavD(i))+';' +
026600040329     C                             %editw(evbDEV:'    /  /  ')+
026700040329     C                             ';'+%editw(tasLNA:'   ')
026800040210     C                   endif
026900040210     C                   endif
027000040210     C                   endif
027100040210     C                   if        vawDTA <> *blanks
027200040210     C                   write(e)  TIVAW000
027300040210     C                   endif
027400040210     C                   eval      vawDTA = wvawDTA
027500040210     C                   endif
027600040210     C                   endif
027700040210     C***                leave
027800040210     C***  KeyEVB01P     readpe    fnevb01l
027900040210     C***                enddo
028000040112     C                   else
028100040112     C                   eval      vawDTA = %trim(vindta) +
028200040209     C                             ';DA CONSEGNARE;'      +
028300040329     C                             %editw(datcor:'    /  /  ')+
028400040329     C                             ';'+%editw(tasLNA:'   ')
028500040112     C                   endif
028600040112     C                   endif
028700040112     C                   endif
028800040112     C                   endif
028900040112     C                   endif
029000040109     C                   endif
029100040112     C*
029200040109     C     KeyTAS32C     reade     titas32c
029300040109     C                   enddo
029400040109     C                   else
029500040109     C                   eval      vawDTA = %trim(vindta) +
029600040329     C                             ';RIFERIMENTO NON TROVATO;'
029700040109     C                   endif
029800040109     C                   endif
029900040113     C*
030000040113     C                   else
030100040113     C                   seton                                        31
030200040113     C                   endif
030300040109     C*
030400040109     C                   else
030500040329     C                   eval      vawDTA = %trim(vindta) + ';STATO;DATA STATO'+
030600040329     C                                      ';LINEA DI ARRIVO'
030700040109     C                   eval      vinflg = '1'
030800040109     C                   endif
030900040113     C*
031000040209     C* Gestisco eventuali casistiche "particolari"
031100040810     C* ...RMN nn presente
031200040810     C                   if        %len(%trim(wRMN)) = *zeros
031300040810     C                   eval      vawDTA = %trim(vindta) +
031400040810     C                             ';DDT NON INDICATO;'
031500040810     C                   else
031600040209     C* ...RMN nn corretto
031700040209     C                   if        *in31 = *on
031800040209     C                   eval      vawDTA = %trim(vindta) +
031900040209     C                             ';RIFERIMENTO NON TROVATO;'
032000040209     C                   endif
032100040810     C                   endif
032200040209     C* ...nn si è riuscito a stabilire 1 STATO preciso alla bolla => genericamente 'DA CONS.'
032300040209     C                   if        vawDTA = *blanks
032400040209     C                   eval      vawDTA = %trim(vindta) +
032500040209     C                             ';DA CONSEGNARE;'      +
032600040329     C                             %editw(datcor:'    /  /  ')+
032700040329     C                             ';'+%editw(tasLNA:'   ')
032800040209     C                   endif
032900040209     C*
033000040209     C* Quindi scarico il buffer del file d output da restituire al cliente
033100040209     C* N31              write(e)  TIVAW000
033200040209     C                   write(e)  TIVAW000
033300990910     C*
033400040112     C* Gestisco l'eventuale errore in scrittura file d output
033500040112     C                   if        %error
033600040112     C                   eval      FlgErr = '1'
033700040112     C                   endif
033800040112     C*
033900040109     C                   if        *in31 = *off
034000990910     C                   eval      vinflg = '1'
034100990910     C                   else
034200990910     C                   eval      vinflg = '2'
034300990910     C                   endif
034400000905     C*
034500000905     C                   else
034600000905     C                   eval      vinflg = '1'
034700000905     C                   endif
034800000905     C*
034900000905     C  N70              update    tivin000
035000040113     C*
035100040113     C                   endif                                                  Fine file (*in70)
035200000905     C*
035300040109     C  N70              enddo
035400040109     C*
035500040109     C* Se nessun errore rilevato
035600040112     C                   if        FlgErr = '0'
035700990910     C* ... restituisco esito OK.
035800990921     C                   eval      wrkesito = '0'
035900990910     C                   else
036000010615     C                   eval      wrkesito = '2'
036100000710     C                   endif
036200990910     C*
036300990914     C                   if        %open(tivin00r)
036400990908     C                   close     tivin00r
036500990914     C                   endif
036600040109     C                   if        %open(tivawwwt)
036700040109     C                   close     tivawwwt
036800990914     C                   endif
036900990920     C*
037000910830     C                   ENDSR
037100000613     C***
037200040113
037300040113
037400040113
037500040113     C*----------------------------------------------------*
037600040113     C*  CONTROLLO NUMERICITA' CAMPI
037700040113     C*----------------------------------------------------*
037800040113     C     CHKNUM        BEGSR
037900040113     C*
038000040113     C                   IF        PiDecChr = *blanks
038100040113     C                   EVAL      PiDecChr = ','
038200040113     C                   ENDIF
038300040113     C*
038400040113     C                   CALL(e)   'ISNUMERIC'
038500040113     C                   PARM                    PiStr            30
038600040113     C                   PARM                    PiDecChr          1
038700040113     C                   PARM      *ZEROS        PiVal            30 9
038800040113     C                   PARM      '0'           PiInt             1
038900040113     C                   PARM      '0'           PiNum             1
039000040113     C                   IF        %error
039100040113     C                   EVAL      PiNum=*off
039200040113     C                   ENDIF
039300040113     C*
039400040113     C                   ENDSR
039500040113     C***
039600040112
039700040112
039800040112
039900040112
040000040112      /TITLE Operazioni d scrittura record d log DOWNLOAD
040100040112     C     wrivlt        BEGSR
040200040112     C*
040300040112     C* Se TUTTA elaborazione è ok => scrivo record che innesta iter DOWNLOAD
040400040112     C                   if        wrkesito = '0'
040500040112     C                   clear                   tivlt000
040600040112     C                   eval      vltksc = vlrksc
040700040112     C                   eval      vltksu = vlrksc
040800040206     C                   eval      vlttip = 'UD'
040900040112     C                   eval      vltfld = vlrfou
041000040112     C                   eval      vltmbd = wMBT
041100040112     C                   eval      vltpgi = wPGT
041200040112     C                   eval      vltsnd = 'D'
041300040112     C                   eval      vltsts = '0'
041400040112     C                   eval      vltfls = *blanks
041500040112     C                   eval      vltmbs = *blanks
041600040112     C                   eval      vltdat = vlrdat
041700040112     C                   eval      vltora = vlrora
041800040112     C                   eval      vltmsg = *blanks
041900040112     C                   eval      vltsqz = *zeros
042000040112     C                   eval      vltisv = 'OE'
042100040112     C                   eval      vlttia = 'F'
042200040112     C* Stacco un progressivo x il download
042300040112     C                   exsr      calprog
042400170324     C                   eval      vltprg = f_tis7prgf
042500040112     C*
042600040112     C                   eval      vltfl1 = *blanks
042700040112     C                   eval      vltfl2 = '01'
042800040112     C                   eval      vltfl3 = *blanks
042900040112     C*
043000040112     C                   write     tivlt000
043100040112     C                   endif
043200040112     C*
043300040112     C                   ENDSR
043400040112     C***
043500040112
043600040112
043700040112
043800040112      /TITLE Valorizzazione Progressivo Applicazione
043900040112     C     calprog       BEGSR
044000040112     C*
044100170324     C     *start        setll     tis7prgf
044200040112     C                   read(e)   tis7prgf
044300040112     C                   if        not %error
044400040112     C                   eval      dwlprg = f_tis7prgf
044500040112     C*
044600040112     C                   move(p)   dwlprg        wrkprg
044700040112     C                   add       1             wrkprg
044800040112     C                   move(p)   wrkprg        dwlprg
044900040112     C                   movel     vltisv        dwlprg
045000040112     C*
045100040112     C                   eval      f_tis7prgf = dwlprg
045200040112     C                   update    tis7prg0
045300170324     C                   else
045400170324     C                   eval      f_tis7prgf = 'XX00000000'
045500040112     C                   endif
045600040112     C*
045700040112     C                   ENDSR
045800010601
045900010601
046000010601
046100010601
046200031229      /TITLE Operazioni iniziali e impostazoini d default
046300010601     C     opeini        BEGSR
046400040112     C*
046500040112     C* Verifico se impostato il parametro relativo al nome pgm traduttore
046600040112     C                   if        %subst(vlrppt:1:10) <> *blanks
046700040112     C                   eval      wPGT = %subst(vlrppt:1:10)
046800040112     C                   else
046900040112     C                   eval      wPGT = 'TITV86R'                             * default
047000040112     C                   endif
047100040112     C*
047200040112     C* Verifico se impostato il nome membro del file di output
047300040112     C                   if        %subst(vlrppt:11:10) <> *blanks
047400040112     C                   eval      wMBT = %subst(vlrppt:11:10)
047500040112     C                   else
047600040112     C                   eval      wMBT = vlrprg                                * default
047700040112     C                   endif
047800040112     C*
047900040112     C* Verifico se impostato altri parametri da passare al traduttore precedentemete indicato
048000040112     C                   if        %subst(vlrppt:21:30) <> *blanks
048100040112     C                   eval      wPPT = %subst(vlrppt:21:30)
048200040205     C* Scompongo ulteriormente la parte d parametro "variabile"
048300040205     C                   eval      wPPTKSC = %subst(wPPT:1:7)
048400040112     C                   endif
048500040112     C*
048600040112     C* Carico le tabelle occorrenti
048700040112     C                   exsr      cartab
048800040112     C*
048900010601     C                   ENDSR
049000010601     C***
049100040112
049200040112
049300040112
049400040112     C     cartab        BEGSR
049500040112     C*
049600040112     C                   clear                   sk2Alav
049700040210     C                   clear                   sk2AlavD
049800040210     C                   clear                   sk2Agiac
049900040210     C                   clear                   sk2AgiacD
050000040210     C                   z-add     *zeros        i                 3 0
050100040112     C                   z-add     *zeros        j                 3 0
050200040112     C                   eval      tblKUT = 1
050300040112     C                   eval      tblCOD = '2A'
050400040112     C     KeyTABP       setll     tabel00f
050500040112     C     KeyTABP       reade     tabel00f
050600040112     C                   dow       not %eof(tabel00f)
050700040112     C                   movel(p)  tblUNI        ds2a
050800040210     C                   if        §2aFTC = 'G'
050900040210     C                   add       1             i
051000040210     C                   eval      sk2Agiac(i)  = tblKEY
051100040210     C                   eval      sk2AgiacD(i) = §2ADES
051200040210     C                   endif
051300040112     C                   if        §2aFTC = 'A'
051400040112     C                   add       1             j
051500040210     C                   eval      sk2Alav(j)  = tblKEY
051600040210     C                   eval      sk2AlavD(j) = §2ADES
051700040112     C                   endif
051800040112     C     KeyTABP       reade     tabel00f
051900040112     C                   enddo
052000040112     C*
052100040112     C                   ENDSR
052200010601
052300010601
052400990910
052500000613     C     *inzsr        BEGSR
052600990910     C*
052700990910     C     *entry        plist
052800990920     C                   parm                    tivlrds
052900990921     C                   parm      wrkesito      esito
053000000724     C                   parm                    prmlit
053100000710     C                   parm                    prmfir
053200040209     C*
053300040209     C* CALCOLA LA DATA CORRENTE
053400040209     C                   time                    wn14             14 0
053500040209     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053600040209     C                   z-add     wn8           g08dat
053700040209     C                   z-add     *zeros        g08inv
053800040209     C                   movel     '0'           g08err
053900040209     C                   call      'XSRDA8'
054000040209     C                   parm                    wlbda8
054100040209     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054200040109     C*
054300040109     C* DEFINIZIONE CHIAVI
054400040112     C*
054500040112     C* Chiave su TABEL00F - Parziale
054600040112     C     KeyTABP       klist
054700040112     C                   kfld                    tblKUT
054800040112     C                   kfld                    tblCOD
054900040109     C*
055000040109     C* Chiave su TITAS32C - Completa
055100040109     C     KeyTAS32C     klist
055200040112     C                   kfld                    tasRMN
055300040112     C                   kfld                    tasCCM
055400040112     C*
055500050310     C* Chiave su TIGCP51L - Completa
055600050310     C     KeyGCP51C     klist
055700040112     C                   kfld                    tasAAS
055800040112     C                   kfld                    tasLNP
055900040112     C                   kfld                    tasNRS
056000040112     C                   kfld                    tasNSP
056100040112     C                   kfld                    gcpFRG
056200040112     C*
056300040112     C* Chiave su FNEVB01L - parziale
056400040112     C     KEYevb01P     klist
056500040112     C                   kfld                    tasAAS
056600040112     C                   kfld                    tasLNP
056700040112     C                   kfld                    tasNRS
056800040112     C                   kfld                    tasNSP
056900000613     C*
057000000613     C                   ENDSR
057100000613     C***
