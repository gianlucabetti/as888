000100091022      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400091015     Fazorg01l  if   e           k disk
000500091013     FTITV1S41L uF   E           k DISK
000600091217     FTABEL00F  IF   E           K DISK
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D tivlrds       e ds                  extname(tivlr00f)
001200100113     D titv1s4flo    e ds
001300091217     D dspr          e ds
001400990910     D esito           s              1
001500000724     D prmlit          s             10
001600000710     D prmfir          s             10
001700990921     D wrkesito        s                   like(esito)
001800091015     D curSped         ds
001900091015     D   TV1S4KEY
002000091015     D   vabAAS
002100091015     D   vabLNP
002200091015     D   vabNRS
002300091015     D   vabNSP
002400091015     D depSped         ds                  likeds(curSped)
002500091111     D cntCurSped      s              2  0 inz
002600091111     D wNumSped        s              6  0 inz
002700091111     D S_VABRSD        s                   like(VABRSD) inz
002800101125     D S_VABRMA        s                   like(VABRMA) inz
002900091217     D skPRprv         s              2    dim(300)
003000091217     D skPRreg         s              3    dim(300)
003100091217     D i               s              3  0 inz
003200091217     D wFil            s                   like(VABLNP) inz
003300100312
003400100312
003500100312     D*------------------
003600100312     D* LINKING A DEFINIZIONI ESTERNE
003700100312     D*------------------
003800100312     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003900100312     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004000010201
004100081217
004200091014     C*
004300990921     C                   reset                   esito
004400990921     C                   reset                   wrkesito
004500000613     C*
004600091217     C                   EXSR      CARTAB
004700091110     C                   EXSR      DEFCAM
004800091013     C                   EXSR      ELAFILE
004900000616     C*
005000000801     C
005100010201     C                   seton                                        LR
005200990908
005300000801
005400910830     C*--------------------------------------------------------
005500091013     C* ELAFILE  LEGGE TITV1S40F ED EFFETTUA CONSIDERAZIONI
005600910830     C*--------------------------------------------------------
005700091014     C     ELAFILE       BEGSR
005800990910     C*
005900091014     C                   clear                   §CTROK            5 0
006000091014     C                   clear                   §CTRNO            5 0
006100091013     C*
006200100312     C                   EVAL      TV1S4KEY  = '0'+%editc(wCCM1:'X')+'_BOLLE'
006300990910     C*
006400091013     C     TV1S4KEY      setll     TITV1S41L
006500091013     C                   if        %found(TITV1S41L)
006600091013     C     TV1S4KEY      reade     TITV1S41L
006700091013     C                   dow       not %eof(TITV1S41L)
006800100113     C*
006900100113     C* Reperisco i flag operativi...
007000100113     C                   eval      titv1s4flo = TV1S4FLO
007100000801     C*
007200091013     C                   if        TV1S4VAB = 'X' and
007300091013     C                             TV1S4VAT = 'X'
007400091015     C*
007500091015     C* Verifico la rottura d codice x cli+sped
007600091111     C                   if        curSped = depSped
007700091111     C                   add       1             wNumSped
007800091111     C                   else
007900091111     C*
008000091111     C* Se tutti i colli sono in conto deposito => avviso
008100091111     C                   if        cntCurSped > *zeros AND
008200091111     C                             cntCurSped = wNumSped + 1
008300091111     C                   exsr      sndeml
008400091111     C                   endif
008500091111     C*
008600091015     C                   setoff                                       55
008700091111     C                   z-add     *zeros        wNumSped
008800091015     C                   z-add     *zeros        cntCurSped
008900091015     C                   eval      depSped = curSped
009000091015     C                   endif
009100091015     C*
009200091015     C***********
009300091015     C* Verifico particolarità x spedizioni in conto deposito a Messina
009400091015     C***                if        TV1S4FLGT =  *blanks AND
009500091015     C***                          TV1S4FLGD =  'S'
009600091015     C                   if        TV1S4FLGD =  'S'
009700091015     C                   seton                                        55
009800091015     C                   add       1             cntCurSped
009900091015     C                   endif
010000091015     C*
010100091021     C***                if        TV1S4FLGT =  *blanks AND
010200091022     C                   if        TV1S4FLGD =  *blanks AND
010300091022     C                             *in55
010400091104     C                   eval      VABNT2 = '+ '                   +
010500091104     C                                      %editc(cntCurSped:'X') +
010600091104     C                                      ' COLLI IN C-DEP'
010700091015     C                   endif
010800091015     C***********
010900091015     C*
011000091015     C                   exsr      impvab                                       => carico  VAB
011100091014     C*
011200091014     C* Aggiorno il record...
011300091014     C                   if        errore <> *zeros
011400091014     C                   eval      TV1S4VAB = '*'
011500091014     C                   eval      TV1S4VAT = '*'
011600091014     C                   endif
011700100113     C*
011800100113     C* Aggiorno flag operativi
011900100113     C                   eval      TV1S4FLO = titv1s4flo
012000091014     C*
012100100113     C                   update    TITV1S400
012200000905     C*
012300050628     C                   endif
012400000905     C*
012500091013     C     TV1S4KEY      reade     TITV1S41L
012600091013     C*
012700091013     C                   ENDdo
012800010202     C*
012900010202     C                   endif
013000091014     C*
013100990910     C* Se non ci sono record con errori ...
013200000710     C                   if        §ctrno = 0
013300990910     C* ... restituisco esito OK.
013400990921     C                   eval      wrkesito = '0'
013500990910     C                   else
013600091014     C                   if        §ctrok > 0
013700990921     C                   eval      wrkesito = '1'
013800000710     C                   else
013900000710     C                   eval      wrkesito = '2'
014000990910     C                   endif
014100000710     C                   endif
014200990920     C*
014300910830     C                   ENDSR
014400000613     C***
014500091013
014600091013     C*----------------------------------------------------*
014700091013     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
014800091013     C*----------------------------------------------------*
014900091013     C     INZVAR        BEGSR
015000091013     C*
015100091013     C*
015200091013     C                   ENDSR
015300091013
015400000801     C*----------------------------------------------------*
015500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
015600000801     C*----------------------------------------------------*
015700000801     C     DEFCAM        BEGSR
015800000801     C*
015900020619     C* Imposto i valori di default...
016000020619     C* ... e poi verifico se sono stati passati come parametri
016100100312     C                   IF        vlrppt > *blanks
016200100312     C                   IF        %subst(vlrppt:1:7) <> *blanks
016300100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
016400100312     C                   EXSR      CHKNUM
016500100312     C                   IF        PiInt=*on
016600100312     C                   Z-ADD     PiVal         wCCM1             7 0
016700100312     C                   ENDIF
016800100312     C                   ENDIF
016900100312     C                   IF        %subst(vlrppt:8:7) <> *blanks
017000100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:7))
017100100312     C                   EXSR      CHKNUM
017200100312     C                   IF        PiInt=*on
017300100312     C                   Z-ADD     PiVal         wCCM2             7 0
017400100312     C                   ENDIF
017500100312     C                   ENDIF
017600100312     C                   IF        %subst(vlrppt:15:7) <> *blanks
017700100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:15:7))
017800100312     C                   EXSR      CHKNUM
017900100312     C                   IF        PiInt=*on
018000100312     C                   Z-ADD     PiVal         wCCM3             7 0
018100100312     C                   ENDIF
018200100312     C                   ENDIF
018300100312     C                   IF        %subst(vlrppt:22:3) <> *blanks
018400100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:22:3))
018500100312     C                   EXSR      CHKNUM
018600100312     C                   IF        PiInt=*on
018700100312     C                   Z-ADD     PiVal         VABLNP
018800100312     C                   ENDIF
018900100312     C                   ENDIF
019000100312     C                   IF        %subst(vlrppt:25:3) <> *blanks
019100100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:25:3))
019200100312     C                   EXSR      CHKNUM
019300100312     C                   IF        PiInt=*on
019400100312     C                   Z-ADD     PiVal         VABCTR
019500100312     C                   ENDIF
019600100312     C                   ENDIF
019700100312     C                   IF        %subst(vlrppt:28:2) = 'VB'
019800100312     C                   SETON                                        50
019900100312     C                   ELSE
020000100312     C                   SETOFF                                       50
020100100312     C                   ENDIF
020200100312     C                   IF        %subst(vlrppt:30:1) = 'L'
020300100312     C                   SETON                                        51
020400100312     C                   ELSE
020500100312     C                   SETOFF                                       51
020600100312     C                   ENDIF
020700100312     C                   IF        %subst(vlrppt:31:8) <> *blanks
020800100312     C                   MOVEL     *blanks       wCMR              8
020900100312     C                   EVAL      wCMR=%trim(%subst(vlrppt:31:8))
021000100312     C                   ENDIF
021100100312     C                   IF        %subst(vlrppt:39:8) <> *blanks
021200100312     C                   MOVEL     *blanks       wEMAIL            8
021300100312     C                   EVAL      wEMAIL=%trim(%subst(vlrppt:39:8))
021400100312     C                   ENDIF
021500100312     C                   ENDIF
021600000801     C*
021700000801     C                   ENDSR
021800091013
021900000801     C*----------------------------------------------------*
022000091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
022100000801     C*----------------------------------------------------*
022200091013     C     IMPVAB        BEGSR
022300010305     C*
022400000801     C                   Z-ADD     *zeros        errore            1 0
022500091013     C*
022600091013     C* Effettuo le considerazioni x determinare il codice cliente mittente
022700091013     C                   select
022800091013     C*
022900091013     C                   when      TV1S4FLGT =  *blanks AND
023000091013     C                             TV1S4FLGD =  *blanks
023100100312     C                   eval      VABCCM = wCCM1
023200100312     C                   eval      VATCCM = wCCM1
023300091013     C*
023400091013     C                   when      TV1S4FLGT =  'A'     AND
023500091013     C                             TV1S4FLGD =  *blanks
023600100312     C                   eval      VABCCM = wCCM2
023700100312     C                   eval      VATCCM = wCCM2
023800091013     C*
023900091015     C                   when      TV1S4FLGD =  'S'
024000091111     C                   eval      S_VABRSD = VABRSD
024100101125     C                   eval      S_VABRMA = VABRMA
024200100312     C                   eval      VABCCM = wCCM3
024300100312     C                   eval      VATCCM = wCCM3
024400091015     C                   eval      VABFFD = 'S'
024500091015     C                   eval      VABTC1 = *blanks
024600091217     C* Effettuo forzature x i "conti depositi" x i quali
024700091217     C* forzo come CAD e LOD quelli della filiale d zona prescelta
024800091217     C                   z-add     1             i
024900091217     C     VABPRD        lookup    skPRprv(i)                             56
025000091217     C                   if        %found
025100091217     C                   select
025200100121     C* ...Sicilia
025300091217     C                   when      skPRreg(i) = '190'
025400091217     C                   eval      wFil = 030
025500100121     C* ..."Triveneto"
025600091217     C                   when      skPRreg(i) = '050' or
025700091217     C                             skPRreg(i) = '060' or
025800091217     C                             skPRreg(i) = '070'
025900100115     C                   eval      wFil = 139
026000100312     C   51              eval      §TV1S4FL01 = 'S'
026100100121     C                   eval      §TV1S4FL02 = 'PD 208'
026200100121     C* ...Emilia Romagna
026300100121     C                   when      skPRreg(i) = '080' or
026400100121     C                             skPRreg(i) = '090'
026500100121     C                   eval      wFil = 001
026600100312     C   51              eval      §TV1S4FL01 = 'S'
026700100121     C                   eval      §TV1S4FL02 = 'BO 207'
026800100121     C*
026900091217     C                   other
027000091217     C                   eval      wFil = vlrpoi
027100091217     C                   endsl
027200091217     C                   endif
027300091217     C*
027400091217     C     wFIL          chain     azorg01l
027500091015     C                   if        %found
027600091015     C                   eval      VABPRD = orgPRO
027700091015     C                   eval      VABLOD = orgLOC
027800091015     C                   movel(p)  orgCPF        VABCAD
027900100115     C                   eval      VABIND = 'Presso Fil. ' +
028000100115     C                                      %char(orgFIL) +
028100091217     C                                      ' ' + %trim(orgdes)
028200100121     C*
028300100121     C                   if        §TV1S4FL01 = 'S'
028400100121     C                   eval      VABIND = 'Presso LOGISTICA ' + §TV1S4FL02
028500100115     C                   endif
028600091015     C                   endif
028700091013     C*
028800091015     C***                when      TV1S4FLGT =  'A'     AND
028900091015     C***                          TV1S4FLGD <> *blanks
029000091015     C***                z-add     1             errore
029100091013     C*
029200091015     C***                when      TV1S4FLGT <> *blanks AND
029300091015     C***                          TV1S4FLGD =  'S'
029400091015     C***                z-add     1             errore
029500091013     C*
029600091013     C                   endsl
029700010202     C*
029800000801     C* Ebbene...
029900010201     C                   IF        errore <> *zeros
030000000801     C                   ADD       1             §CTRNO
030100000801     C                   ELSE
030200091014     C                   ADD       1             §CTROK
030300000801     C                   ENDIF
030400000801     C*
030500000801     C                   ENDSR
030600090331
030700090331
030800090331
030900070411     C     *pssr         BEGSR
031000070411     C*
031100070411     C                   eval      wrkesito = '2'
031200070411     C*
031300070411     C                   seton                                        LR
031400070411     C*
031500070411     C                   ENDSR     '*CANCL'
031600070411     C***
031700070411
031800091217
031900091111     c*--------------------------------------------------------------------------------------------*
032000091111     c* /TITLE Compongo il testo e spedisco una e-m@ail
032100091111     c*--------------------------------------------------------------------------------------------*
032200091111     c     sndeml        begsr
032300091111     c*
032400091111     c* Inizializzo variabili
032500091111     c                   movel     *blanks       wrkEml          253
032600091111     c                   movel     *blanks       wrkEmlMsg      5000
032700091111     c                   movel     *blanks       wrkEmlOgg        44
032800091111     c* Valorizzo i campi della e-m@ail
032900120309     c                   eval      wrkEml=%trim(wEMAIL)+'@brt.it '
033000100312     c                   eval      wrkEmlOgg=%trim(wCMR)+' - ATTENZIONE !!!'
033100091111     c                   EVAL      wrkEmlMsg = 'Tutti i colli sono in conto '  +
033200091111     c                             'deposito.'                           +':/N'+
033300091111     c                                                                    ':/N'+
033400091111     c                             'Verificare la seguente spedizione - '      +
033500091111     c                             'aggiornare note bolla - '                  +
033600091111     c                             'abbinare DDT ad altra spedizione '         +
033700091111     c                             'per medesimo destinatario.'          +':/N'+
033800091111     c                                                                    ':/N'+
033900101125     c                             'Rif.Mittente: ' + S_VABRMA           +':/N'+
034000091111     c                             'Destinatario: ' + S_VABRSD           +':/N'+
034100091111     c                             ' '                                   +':/N'
034200091111     c*
034300091111     c                   call(e)   'TIS701C'
034400091111     c                   parm                    wrkEml
034500091111     c                   parm                    wrkEmlOgg
034600091111     c                   parm                    wrkEmlMsg
034700091111     c*
034800091111     c                   endsr
034900091217
035000091217
035100091217
035200091217     C     cartab        BEGSR
035300091217     C*
035400091217     C* Carico causali evento divise x categoria (A=Lavv G=Giac)
035500091217     C                   clear                   skPRprv
035600091217     C                   clear                   skPRreg
035700091217     C                   z-add     *zeros        i
035800091217     C                   eval      tblKUT = 1
035900091217     C                   eval      tblCOD = 'PR'
036000091217     C     KEYtabP       setll     tabel00f
036100091217     C     KEYtabP       reade     tabel00f
036200091217     C                   dow       not %eof(tabel00f)
036300091217     C                   movel(p)  tblUNI        dsPR
036400091217     C                   add       1             i
036500091217     C                   eval      skPRprv(i) = tblKEY
036600091217     C                   eval      skPRreg(i) = §PRCRE
036700091217     C     KEYtabP       reade     tabel00f
036800091217     C                   enddo
036900091217     C*
037000091217     C                   ENDSR
037100100312
037200100312
037300091217
037400100312     C*----------------------------------------------------*
037500100312     C*  CONTROLLO NUMERICITA' CAMPI
037600100312     C*----------------------------------------------------*
037700100312     C     CHKNUM        BEGSR
037800100312     C*
037900100312     C                   IF        PiDecChr = *blanks
038000100312     C                   EVAL      PiDecChr = ','
038100100312     C                   ENDIF
038200100312     C*
038300100312     C                   callp(e)  UBISNUM_Check(PiStr
038400100312     C                                          :PiDecChr
038500100312     C                                          :PiVal
038600100312     C                                          :PiNum
038700100312     C                                          :PiInt)
038800100312     C*
038900100312     C                   IF        %error
039000100312     C                   EVAL      PiInt=*off
039100100312     C                   ENDIF
039200100312     C*
039300100312     C                   ENDSR
039400100312     C***
039500090331
039600990910
039700000613     C     *inzsr        BEGSR
039800990910     C*
039900990910     C     *entry        plist
040000990920     C                   parm                    tivlrds
040100990921     C                   parm      wrkesito      esito
040200000724     C                   parm                    prmlit
040300000710     C                   parm                    prmfir
040400091217     C*
040500091217     C* Chiave su TABEL00F - parziale
040600091217     C     KEYtabP       KLIST
040700091217     C                   KFLD                    tblKUT
040800091217     C                   KFLD                    tblCOD
040900000830     C*
041000000613     C                   ENDSR
041100000613     C***
