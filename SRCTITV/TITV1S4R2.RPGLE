000100091021      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200110324     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBBNDDIR') ACTGRP('BARTVAS')
000300990908
000400091015     FTITV1S41L uF   E           k DISK    prefix(W_)
000500091021     FEDIVABwr  O    E             DISK    usropn
000600091021     FEDIVATwr  O    E             DISK    usropn
000700100113     FSP3PREP   O    e             disk    usropn
000800100113     F                                     extfile(LibFilePRE)
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990920     D dscmz         e ds                  inz
001400990910     D psds           sds
001500990910     D  procname         *PROC
001600990920     D tivlrds       e ds                  extname(tivlr00f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100010202     D parccm          s              8    INZ(*blanks)
002200010202     D parmbr          s             10    INZ(*blanks)
002300010202     D paropz          s              1    INZ(*blanks)
002400010202     D chkcall         s              1    INZ(*blanks)
002500091015     D titv1s4ds_wk  e ds                  extname(titv1s40f)
002600091015     D                                     prefix(W_)
002700091015     D titv1s4ds_ok  e ds                  extname(titv1s40f)
002800091015     D curSped         ds                  qualified
002900091015     D   TV1S4KEY                          like(W_TV1S4KEY)
003000091015     D   vabCCM                            like(W_vabCCM)
003100091015     D   vabAAS                            like(W_vabAAS)
003200091015     D   vabLNP                            like(W_vabLNP)
003300091015     D   vabNRS                            like(W_vabNRS)
003400091015     D   vabNSP                            like(W_vabNSP)
003500091013     D depSped         ds                  likeds(curSped)
003600091015     D wVABNCL         s                   like(VABNCL)
003700091015     D wVABPKB         s                   like(VABPKB)
003800100113     D LibFilePRE      s             21A   inz
003900100113     D titv1s4flo    e ds
004000010201
004100100113
004200100113     D*------------------
004300100113     D* LINKING A DEFINIZIONI ESTERNE
004400100113     D*------------------
004500100113     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
004600100113     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
004700100312
004800100312
004900100312     D*------------------
005000100312     D* LINKING A DEFINIZIONI ESTERNE
005100100312     D*------------------
005200100312     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005300100312     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005400100312
005500100113
005600100113
005700100113     C*
005800100113     C                   setoff                                       54
005900100113     C*
006000100113     C* Reperisco il sistema AS/400 corrente
006100100113     C                   callp     UBRTVNETA_Rtv(PiOutSysNeta)
006200100113     C                   if        %subst(PiOutSysNeta:1:6) = 'SETRAS'
006300100113     C                   eval      LibFilePRE = 'GEODETRPRE/SP3PREP'
006400100113     C                   else
006500100113     C                   seton                                        54
006600100113     C                   endif
006700100113     C*
006800100113     C                   if        not %open(SP3PREP)
006900100113     C                   open(e)   SP3PREP
007000100113     C                   if        %error
007100100113     C                   seton                                        54
007200100113     C                   endif
007300100113     C                   endif
007400100113     C*
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700000613     C*
007800091110     C                   EXSR      DEFCAM
007900040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
008000000613     C*
008100010202     C* Effettuo la chiamata al CLLE preposto
008200100316     C                   if        vlrtip = 'VB' or
008300100316     C                             vlrtip = 'AB'
008400091021     C                   call(e)   'TITVEVTC'
008500010202     C                   parm                    parccm
008600010202     C                   parm                    parmbr
008700010202     C                   parm      '2'           paropz
008800091014     C                   endif
008900091014     C*
009000100316     C                   if        vlrtip = 'VT' or
009100100316     C                             vlrtip = 'AT'
009200091021     C                   call(e)   'TITVEVBC'
009300091014     C                   parm                    parccm
009400091014     C                   parm                    parmbr
009500091014     C                   parm      '2'           paropz
009600091014     C                   endif
009700000616     C*
009800000801     C
009900010201     C                   seton                                        LR
010000000613
010100091110
010200091110     C*----------------------------------------------------*
010300091110     C*  IMPOSTAZIONE CAMPI COSTANTI
010400091110     C*----------------------------------------------------*
010500091110     C     DEFCAM        BEGSR
010600091110     C*
010700091110     C* Imposto i valori di default...
010800091110     C* ... e poi verifico se sono stati passati come parametri
010900100312     C                   IF        vlrppt > *blanks
011000100312     C                   IF        %subst(vlrppt:1:7) <> *blanks
011100100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
011200100312     C                   EXSR      CHKNUM
011300100312     C                   IF        PiInt=*on
011400100312     C                   Z-ADD     PiVal         wCCM1             7 0
011500100312     C                   ENDIF
011600100312     C                   ENDIF
011700100312     C                   IF        %subst(vlrppt:8:7) <> *blanks
011800100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:7))
011900100312     C                   EXSR      CHKNUM
012000100312     C                   IF        PiInt=*on
012100100312     C                   Z-ADD     PiVal         wCCM2             7 0
012200100312     C                   ENDIF
012300100312     C                   ENDIF
012400100312     C                   IF        %subst(vlrppt:15:7) <> *blanks
012500100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:15:7))
012600100312     C                   EXSR      CHKNUM
012700100312     C                   IF        PiInt=*on
012800100312     C                   Z-ADD     PiVal         wCCM3             7 0
012900100312     C                   ENDIF
013000100312     C                   ENDIF
013100100312     C                   IF        %subst(vlrppt:22:3) <> *blanks
013200100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:22:3))
013300100312     C                   EXSR      CHKNUM
013400100312     C                   IF        PiInt=*on
013500100312     C                   Z-ADD     PiVal         VABLNP
013600100312     C                   ENDIF
013700100312     C                   ENDIF
013800100312     C                   IF        %subst(vlrppt:25:3) <> *blanks
013900100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:25:3))
014000100312     C                   EXSR      CHKNUM
014100100312     C                   IF        PiInt=*on
014200100312     C                   Z-ADD     PiVal         VABCTR
014300100312     C                   ENDIF
014400100312     C                   ENDIF
014500100312     C                   IF        %subst(vlrppt:28:2) = 'VB'
014600100312     C                   SETON                                        50
014700100312     C                   ELSE
014800100312     C                   SETOFF                                       50
014900100312     C                   ENDIF
015000100312     C                   IF        %subst(vlrppt:30:1) = 'L'
015100100312     C                   SETON                                        51
015200100312     C                   ELSE
015300100312     C                   SETOFF                                       51
015400100312     C                   ENDIF
015500100312     C                   IF        %subst(vlrppt:31:8) <> *blanks
015600100312     C                   MOVEL     *blanks       wCMR              8
015700100312     C                   EVAL      wCMR=%trim(%subst(vlrppt:31:8))
015800100312     C                   ENDIF
015900100312     C                   IF        %subst(vlrppt:39:8) <> *blanks
016000100312     C                   MOVEL     *blanks       wEMAIL            8
016100100312     C                   EVAL      wEMAIL=%trim(%subst(vlrppt:39:8))
016200100312     C                   ENDIF
016300100312     C                   ENDIF
016400091110     C*
016500091110     C                   ENDSR
016600990908
016700000801
016800910830     C*--------------------------------------------------------
016900091021     C* RWFILE   LEGGE TITV1S40F E SCRIVE EDIVABWR e EDIVATWR
017000910830     C*--------------------------------------------------------
017100040526     C     RWFILE        BEGSR
017200070103     C*
017300091021     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
017400091014     C                   exsr      prevabt
017500010201     C*
017600010202     C                   if        chkcall = '0'
017700091014     C*
017800091021     C                   if        not %open(edivabwr)
017900091021     C                   open      edivabwr
018000091014     C                   endif
018100010202     C*
018200091021     C                   if        not %open(edivatwr)
018300091021     C                   open      edivatwr
018400010201     C                   endif
018500091013     C*
018600091013     C                   clear                   §CTROKVB          5 0
018700091013     C                   clear                   §CTROKVT          5 0
018800091013     C                   clear                   §CTRNO            5 0
018900100312     C*
019000100312     C                   setoff                                       60
019100100312     C                   setoff                                       61
019200091013     C*
019300100312     C                   EVAL      W_TV1S4KEY  = '0'+%editc(wCCM1:'X')+'_BOLLE'
019400990910     C*
019500091015     C     W_TV1S4KEY    setll     TITV1S41L
019600091013     C                   if        %found(TITV1S41L)
019700091015     C     W_TV1S4KEY    reade     TITV1S41L
019800091013     C                   dow       not %eof(TITV1S41L)
019900000801     C*
020000091013     C* ---- Record buoni da inviare in filiale
020100091015     C                   if        W_TV1S4VAB = 'X' and
020200091015     C                             W_TV1S4VAT = 'X'
020300100312     C                   seton                                        60
020400100312     C                   seton                                        61
020500100113     C*
020600100113     C* Reperisco i flag operativi...
020700100113     C                   eval      titv1s4flo = W_TV1S4FLO
020800091015     C*
020900091015     C                   eval      curSped.TV1S4KEY = W_TV1S4KEY
021000091015     C                   eval      curSped.vabCCM   = W_vabCCM
021100091015     C                   eval      curSped.vabAAS   = W_vabAAS
021200091015     C                   eval      curSped.vabLNP   = W_vabLNP
021300091015     C                   eval      curSped.vabNRS   = W_vabNRS
021400091015     C                   eval      curSped.vabNSP   = W_vabNSP
021500091015     C*
021600070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
021700091013     C                   if        curSped <> depSped
021800100312     C                   setoff                                       60
021900070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
022000091015     C                   if        §CTROKVT > *zeros
022100091013     C                   exsr      wrivab                                       => scarico VAB
022200100312     C                   seton                                        60
022300091015     C                   endif
022400091015     C* Assegno il buffer del formato record corrente
022500091015     C                   eval      titv1s4ds_ok =  titv1s4ds_wk
022600091015     C* Azzero il numero colli in quanto fa fede il numero di dettagli
022700091015     C                   z-add     *zeros        wVABNCL
022800091202     C* Azzero il peso in quanto fa fede il peso dei singoli dettagli (ovviamente in sommatoria)
022900091015     C                   z-add     *zeros        wVABPKB
023000091015     C*
023100091013     C                   exsr      wrivat                                       => scarico VAT
023200100312     C                   setoff                                       61
023300070103     C* Salvo il raggruppamento spedizione cliente corrente
023400091013     C                   eval      depSped = curSped
023500070213     C*
023600070213     C* Se collo successivo x stessa bolla
023700070213     C                   else
023800091015     C* Assegno il buffer del formato record corrente
023900091015     C                   eval      titv1s4ds_ok =  titv1s4ds_wk
024000091015     C*
024100091013     C                   exsr      wrivat                                       => scarico VAT
024200100312     C                   setoff                                       61
024300070103     C                   endif
024400000905     C*
024500091013     C                   delete    TITV1S41L
024600050628     C                   endif
024700091013     C* ----
024800091013     C*
024900091013     C*
025000091013     C* ---- Record con errori (da gestire in qualche modo)
025100091015     C                   if        W_TV1S4VAB = '*' or
025200091015     C                             W_TV1S4VAT = '*'
025300091013     C                   ADD       1             §CTRNO
025400091013     C                   endif
025500091013     C* ----
025600091013     C*
025700091015     C     W_TV1S4KEY    reade     TITV1S41L
025800091013     C*
025900091013     C                   ENDdo
026000091015     C*
026100100312     C* Scarico i buffer rimasti in sospeso
026200110324     C                   if        *in60 OR
026300110324     C                             (§CTROKVT > *zeros AND §CTROKVB = *zeros)
026400091015     C                   exsr      wrivab                                       => scarico VAB
026500091015     C                   endif
026600100312     C                   if        *in61
026700100312     C                   exsr      wrivat                                       => scarico VAT
026800100312     C                   endif
026900010202     C*
027000010202     C                   endif
027100091013     C*
027200091013     C                   endif
027300091013
027400990910
027500990910     C* Se non ci sono record con errori ...
027600000710     C                   if        §ctrno = 0
027700990910     C* ... restituisco esito OK.
027800990921     C                   eval      wrkesito = '0'
027900990910     C                   else
028000010201     C                   if        §ctrokvb > 0
028100990921     C                   eval      wrkesito = '1'
028200000710     C                   else
028300000710     C                   eval      wrkesito = '2'
028400990910     C                   endif
028500000710     C                   endif
028600990910     C*
028700091021     C                   if        %open(edivabwr)
028800091021     C                   close     edivabwr
028900990914     C                   endif
029000091021     C                   if        %open(edivatwr)
029100091021     C                   close     edivatwr
029200010201     C                   endif
029300990910     C*
029400091013     C                   if        (§ctrokvb > 0 or
029500091013     C                              §ctrokvt > 0)
029600000724     C                             and vlrpoi <> *zeros
029700010202     C                   exsr      invio
029800990920     C                   endif
029900990920     C*
030000910830     C                   ENDSR
030100000613     C***
030200010305
030300010305     C*----------------------------------------------------*
030400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030500010305     C*----------------------------------------------------*
030600020305     C     WRIVAB        BEGSR
030700091021     C*
030800091021     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
030900100312     C                   EVAL      VABCMR = %trim(wCMR)+'_'+%char(datcor)
031000091021     C                   EVAL      VABDCM = DATCOR
031100091021     C                   EVAL      VABDTS = DATCOR
031200091021     C                   EVAL      VABHMS = ORACOR
031300091021     C                   EVAL      VABCNT = 1
031400091015     C*
031500091015     C* Imposto il numero colli con il numero dei dettagli conteggiati
031600091015     C                   eval      VABNCL = wVABNCL
031700091015     C* Reimposto il peso
031800091015     C                   eval      VABPKB = wVABPKB
031900010305     C*
032000060225     C* Quindi scarico il buffer del file d testata
032100091021     C                   write     edivab00                                     => scarico il VAB
032200091014     C                   ADD       1             §ctrokvb
032300010305     C*
032400010305     C                   ENDSR
032500091013
032600010201     C*----------------------------------------------------*
032700091021     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
032800010201     C*----------------------------------------------------*
032900020305     C     WRIVAT        BEGSR
033000091021     C*
033100091021     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
033200100312     C                   EVAL      VATCMR = %trim(wCMR)+'_'+%char(datcor)
033300091021     C                   EVAL      VATCNT = 1
033400091015     C*
033500091015     C* Incremento il num_colli sulla testata in relazione al conteggio dei dettagli
033600091015     C                   add       1             wVABNCL
033700091118     C*
033800100312     C* X il traffico "wCCM3" (2494308) considero sempre 5kg x ogni collo
033900091202     C***                add       5             wVABPKB
034000091015     C* ...il peso solo in caso di spedizoini in "conto deposito"
034100091015     C***                if        W_TV1S4FLGT =  *blanks AND
034200091015     C***                          W_TV1S4FLGD =  'S'
034300091202     C***                if        W_TV1S4FLGD =  'S'
034400091202     C                   add       VABPKB        wVABPKB
034500091202     C***                else
034600091127     C***                z-add     VABPKB        wVABPKB
034700091202     C***                endif
034800050628     C*
034900060223     C* Scrivo solo se valorizzato qualcosa
035000060223     C                   IF        VATNOT <> *blanks
035100091021     C                   WRITE     EDIVAT00
035200090922     C                   ADD       1             §ctrokvt
035300100113     C*
035400100113     C* Se richiesto => gestione x scrittura "chi sono" x logistica
035500100312     C                   if        *in51 and not *in54
035600100113     C                   if        §TV1S4FL01 = 'S'  AND
035700100113     C                             VATTRC     = 'E'
035800100113     C                   clear                   SP3PRE
035900100121     C                   eval      S1NEXT = §TV1S4FL02 + ' ' + %char(datcor)
036000100113     C                   eval      S1SYMB = VATNOT
036100100113     C                   write     SP3PRE
036200100113     C                   endif
036300100113     C                   endif
036400100113     C*
036500060223     C                   ENDIF
036600010201     C*
036700010201     C                   ENDSR
036800091013
036900010202     C*----------------------------------------------------*
037000091021     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
037100010202     C*----------------------------------------------------*
037200091014     C     PREVABT       BEGSR
037300010202     C*
037400091021     C* Compongo il nome del membro da dare al EDIVATWR
037500010202     C                   eval      parmbr = vlrhdl
037600010202     C                   movel     'M'           parmbr
037700050627     C                   eval      parccm = %subst(vlrKSC:2:7)
037800010202     C                   eval      paropz = '1'
037900010202     C* Effettuo la chiamata al CLLE preposto
038000100316     C                   if        vlrtip = 'VB' or
038100100316     C                             vlrtip = 'AB'
038200091021     C                   call(e)   'TITVEVTC'
038300010202     C                   parm                    parccm
038400010202     C                   parm                    parmbr
038500010202     C                   parm                    paropz
038600091014     C                   endif
038700091014     C*
038800100316     C                   if        vlrtip = 'VT' or
038900100316     C                             vlrtip = 'AT'
039000091021     C                   call(e)   'TITVEVBC'
039100091014     C                   parm                    parccm
039200091014     C                   parm                    parmbr
039300091014     C                   parm                    paropz
039400091014     C                   endif
039500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039600010202     C                   if        %error
039700010202     C                   movel     '1'           chkcall
039800010202     C                   else
039900010202     C                   movel     '0'           chkcall
040000010202     C                   endif
040100010202     C*
040200010202     C                   ENDSR
040300011113
040400000801
040500000801
040600990920      /TITLE Invio dei dati al punto operativo.
040700010202     C     invio         BEGSR
040800990920     C*
040900091021     C* 1° invio EDIVAT
041000010201     C                   reset                   dscmz
041100010201     C                   move      vlrpoi        cmzdst
041200091021     C                   eval      cmzfld = 'EDIVATWR'
041300010201     C                   eval      cmzmbd = vlrhdl
041400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
041500021009     C***                if        prmfir = *blanks
041600091021     C                   eval      cmzfla = 'EDIVAT0F'
041700091021     C                   eval      cmzmba = 'EDIVAT0F'
041800021009     C***                else
041900021009     C***                eval      cmzfla = prmfir
042000021009     C***                eval      cmzmba = prmfir
042100021009     C***                endif
042200010201     C                   eval      cmznrr = *zeros
042300020305     C                   move      §ctrokvt      cmznrr
042400021018     C                   eval      cmzlba = vlrfl1
042500010201     C                   call(e)   'TIS711C'
042600010201     C                   parm                    dscmz
042700010201     C                   parm      *blanks       esito
042800010205     C                   if        %error
042900010205     C                             or cmzerr = '1'
043000010205     C                             or esito  = '1'
043100010205     C                   eval      wrkesito = '3'
043200010205     C                   else
043300010201     C*
043400091021     C* 2° invio EDIVAB
043500010201     C                   reset                   dscmz
043600010201     C                   move      vlrpoi        cmzdst
043700091021     C                   eval      cmzfld = 'EDIVABWR'
043800010201     C                   eval      cmzmbd = vlrhdl
043900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
044000021009     C***                if        prmfir = *blanks
044100091021     C                   eval      cmzfla = 'EDIVAB0F'
044200091021     C                   eval      cmzmba = 'EDIVAB0F'
044300021009     C***                else
044400021009     C***                eval      cmzfla = prmfir
044500021009     C***                eval      cmzmba = prmfir
044600021009     C***                endif
044700010201     C                   eval      cmznrr = *zeros
044800010201     C                   move      §ctrokvb      cmznrr
044900021018     C                   eval      cmzlba = vlrfl1
045000010201     C                   call(e)   'TIS711C'
045100010201     C                   parm                    dscmz
045200010201     C                   parm      *blanks       esito
045300010201     C                   if        %error
045400010201     C                             or cmzerr = '1'
045500010201     C                             or esito  = '1'
045600010201     C                   eval      wrkesito = '3'
045700010201     C                   endif
045800010205     C                   endif
045900990920     C*
046000000613     C                   ENDSR
046100000613     C***
046200090331
046300090331
046400090331
046500070411     C     *pssr         BEGSR
046600070411     C*
046700091021     C                   if        %open(edivabwr)
046800091021     C                   close     edivabwr
046900070411     C                   endif
047000091021     C                   if        %open(edivatwr)
047100091021     C                   close     edivatwr
047200070411     C                   endif
047300070411     C*
047400070411     C* Effettuo la chiamata al CLLE preposto
047500100316     C                   if        vlrtip = 'VB' or
047600100316     C                             vlrtip = 'AB'
047700091021     C                   call(e)   'TITVEVTC'
047800070411     C                   parm                    parccm
047900070411     C                   parm                    parmbr
048000070411     C                   parm      '2'           paropz
048100091014     C                   endif
048200091014     C*
048300100316     C                   if        vlrtip = 'VT' or
048400100316     C                             vlrtip = 'AT'
048500091021     C                   call(e)   'TITVEVBC'
048600091014     C                   parm                    parccm
048700091014     C                   parm                    parmbr
048800091014     C                   parm      '2'           paropz
048900091014     C                   endif
049000070411     C*
049100070411     C                   eval      wrkesito = '2'
049200070411     C*
049300070411     C                   seton                                        LR
049400070411     C*
049500070411     C                   ENDSR     '*CANCL'
049600070411     C***
049700100312
049800100312
049900100312
050000100312     C*----------------------------------------------------*
050100100312     C*  CONTROLLO NUMERICITA' CAMPI
050200100312     C*----------------------------------------------------*
050300100312     C     CHKNUM        BEGSR
050400100312     C*
050500100312     C                   IF        PiDecChr = *blanks
050600100312     C                   EVAL      PiDecChr = ','
050700100312     C                   ENDIF
050800100312     C*
050900100312     C                   callp(e)  UBISNUM_Check(PiStr
051000100312     C                                          :PiDecChr
051100100312     C                                          :PiVal
051200100312     C                                          :PiNum
051300100312     C                                          :PiInt)
051400100312     C*
051500100312     C                   IF        %error
051600100312     C                   EVAL      PiInt=*off
051700100312     C                   ENDIF
051800100312     C*
051900100312     C                   ENDSR
052000100312     C***
052100100312
052200090331
052300990910
052400000613     C     *inzsr        BEGSR
052500990910     C*
052600990910     C     *entry        plist
052700990920     C                   parm                    tivlrds
052800990921     C                   parm      wrkesito      esito
052900000724     C                   parm                    prmlit
053000000710     C                   parm                    prmfir
053100091021     C*
053200091021     C* CALCOLA LA DATA CORRENTE
053300091021     C                   time                    wn14             14 0
053400091021     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053500091021     C                   movel     wn14          oracor            6 0          *ORA
053600091110     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
053700091110     C                   eval      datcor = %dec(%date() : *ISO)
053800000830     C*
053900000613     C                   ENDSR
054000000613     C***
