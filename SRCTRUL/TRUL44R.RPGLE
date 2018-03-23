000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200900418     H*                                                              *
000300041015     A*  IL PGM REPERISCE USRDFNDTA X COMUNICAZIONI VIA E-MAIL
000400041018     A*  OPZIONALMENTE ESEGUE ANCHE OVRPRTF
000500040630     FTntbe01l  IF   E           K DISK
000600040630     FTfntc01l  IF   E           K DISK
000700041018     Fazorg01l  IF   E           K DISK
000800940922     F*--------------------------------------------------------------*
000900120123     D key1            S             15
001000120123     D key2            S             15
001100041015     D dtaiso          S               d
001200120312     d MITTERR         c                   '@brt.it'
001300000202     D OG143         E DS
001400041015     D OG148         E DS
001500041018     D TRUL44ds      E DS
001600050902     D*TRUL43        E DS                  EXTNAME(TRUL43DS) PREFIX(£)
001700041018     D TRUL43DS      E DS
001800041018     d trtcm1ds      e ds
001900050902     D dstrtcm1      E DS                  EXTNAME(TRtcm1DS) PREFIX(£)
002000041015     d fnlv55ds      e ds
002100040914     D dmra          e DS
002200041015     c                   clear                   trtcm1ds
002300041029     c* se passato destinatario della mail non sto nemmeno a ricercarlo
002400041029     c* nei luoghi o nelle note
002500041029     c                   if        d44des <>' '
002600041029     c                   eval      §cm1dst = d44des
002700041029     c                   else
002800041015     C*  Controllo se esiste un luogo di invio comunicazione
002900041019     c                   if        d44ksc <> 0 and d44cl <> *blanks
003000041018     C                   CLEAR                   TRUL43ds
003100041015     C                   Z-ADD     d44ksc        D43KSC
003200041015     C                   eval      d43cod = d44cl
003300960306     C                   CALL      'TRUL43R'
003400041015     C                   PARM                    TRUL43DS
003500041015      * se esiste il luogo l'indirizzo e-mail deve essere obbligatoriamente
003600041015      * associato al luogo altrimenti è un errore
003700041019     c                   if        d43trv = 'S' and
003800041019     c                             d43trvm = 'S'
003900041015     c                   movel     d43mail       §cm1dst
004000041015     c                   end
004100041019     C* se si vuole anche il ritorno della DS TRUL43DS
004200050902     C*                  IF        %PARMS=2
004300050902     C*                  MOVEL     TRUL43DS      TRUL43
004400050902     C*                  END
004500041015     c                   end
004600041018     c* reperisco e-mail da TFNTC sole se non trovato il luogo o se non
004700041018     c* passato il codice cliente
004800041018     c                   if        d44nk1 <> *blanks and §cm1dst = ' '
004900040707     c     kntc          chain     tfntc01l
005000040707     c                   if        %found
005100040707     c                   movel     ntcrnt        §cm1dst
005200040707     c                   end
005300040707     c                   end
005400041029     c                   end
005500041026      * richiamo per reperimento mittente della mail
005600041026     c                   exsr      repmitt
005700050902     c                   move      d44po         §cm1po
005800050902     c                   move      '0'           §cm1sts
005900050902     c                   movel     §mrareg       §cm1tips
006000050902     c                   movel     §mraidpro     §cm1idp
006100050902     c                   eval      §cm1var = d44obj
006200050902     C* se si è passato anche il secondo parametro (trtcm1ds) con dati
006300050902     C* impostati li sostituisco nel campo di output D44DTA, D44MIT
006400050902     C                   IF        %PARMS=2
006500050902     c                   if        £§cm1mitt <> ' '
006600050902     c                   eval      §cm1mitt = £§cm1mitt
006700050902     c                   eval      d44mit = £§cm1mitt
006800050902     c                   end
006900050902     c                   if        £§cm1dst <> ' '
007000050902     c                   eval      §cm1dst = £§cm1dst
007100050902     c                   end
007200050902     c                   if        £§cm1tips <> ' '
007300050902     c                   eval      §cm1tips = £§cm1tips
007400050902     c                   end
007500050902     c                   if        £§cm1po <> ' '
007600050902     c                   eval      §cm1po = £§cm1po
007700050902     c                   end
007800050902     c                   if        £§cm1var <> ' '
007900050902     c                   eval      §cm1var = £§cm1var
008000050902     c                   end
008100050902     c                   if        £§cm1sts <> ' '
008200050902     c                   eval      §cm1sts = £§cm1sts
008300050902     c                   end
008400050902     c                   if        £§cm1idp <> ' '
008500050902     c                   eval      §cm1idp = £§cm1idp
008600050902     c                   end
008700050902     c                   if        £§cm1prog <> ' '
008800050902     c                   eval      §cm1prog = £§cm1prog
008900050902     c                   end
009000050902     c                   if        £§cm1att <> ' '
009100050902     c                   eval      §cm1att = £§cm1att
009200050902     c                   end
009300050902     c                   if        £§cm1totatt <> ' '
009400050902     c                   eval      §cm1totatt = £§cm1totatt
009500050902     c                   end
009600060721     c                   if        £§cm1pcl <> ' '
009700060721     c                   eval      §cm1pcl = £§cm1pcl
009800060721     c                   end
009900050902     c                   if        £§cm1flo <> ' '
010000050902     c                   eval      §cm1flo = £§cm1flo
010100050902     c                   end
010200050902     C                   END
010300041102     c* non trovato indirizzo mail destinatario invio la mail allo stesso
010400041102     c* utente che sta inviando (solo per note) e segnalo l'anomalia come
010500041102     c* codice di ritorno
010600041102     c                   if        §cm1dst = *blanks and d44nk1 <> *blanks
010700041102     c                   eval      §cm1dst = %trim(§cm1mitt)  + mitterr
010800041102     c                   eval      d44err = 'A'
010900041102     c                   endif
011000041019     c* se ho trovato l'indirizzo procedo altrimenti esco con errore
011100041019     c                   if        §cm1dst = *blanks
011200041019     c                   exsr      esci
011300050902     c                   end
011400050902     c                   eval      d44dta = trtcm1ds
011500050902     c* se richiesta ovrprtf
011600041015     c                   if        d44ovr = 'S'
011700041015     c                   exsr      srovr
011800041015     c                   end
011900041015     c*
012000041015     C                   seton                                        lr
012100040629     C*------------------------------------------------------*
012200041015     C*  Prepara ovrprtf se richiesto
012300040629     C*------------------------------------------------------*
012400041015     C     srovr         begsr
012500041018     c* se non passato OVRSCOPE imposto *ACTGRPDFN
012600041018     c                   if        d44scope = ' '
012700041018     c                   eval      d44scope = '*ACTGRPDFN'
012800041018     c                   end
012900041021     c                   clear                   errore
013000041018     c*
013100041021     C                   CALL      'TRUL44C'                            60
013200041021     C                   PARM                    d44prtf
013300041021     C                   PARM                    §mraoutqi
013400041021     C                   PARM                    d44scope
013500041021     C                   PARM                    d44dta
013600041021     C                   PARM                    errore            1
013700041021     c                   if        *in60 or errore = '1'
013800041015     c                   exsr      esci
013900041015     c                   end
014000041015     c                   endsr
014100041015     C*------------------------------------------------------*
014200041015     C*  Prepara stringa mittente per invio mail o riferimento per FAX
014300041015     C*------------------------------------------------------*
014400041015     C     repmitt       begsr
014500041015      * gestione network x sapere come impostare il mittente
014600041015     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX' or
014700041015     c                             §ogntw = 'FED' or §ogntw = 'EUP'
014800041015     c                   clear                   fnlv55ds
014900041015     c* Imposto i dati in LV55 x reperire terminal partenza fil. estera
015000041015     c                   movel     'P'           d55tpt
015100041015     c                   movel     amgdtu        d55drf
015200041015     c                   movel     d44po         d55lin
015300041015     c                   move      d44po         d55lnp
015400041015     c                   call      'FNLV55R'
015500041015     c                   parm                    fnlv55ds
015600041015     c                   if        d55err <> *blanks
015700041015     c                   exsr      esci
015800041015     c                   end
015900041015      * P.O. estero va inviato come 'INTyyy' yyy=terminal partenza
016000041015     c                   move      d55tfp        terminalp         3
016100131129     c*
016200131129     c                   select
016300131129     c                   when      §mramites = '*OG121' and og121 <> *blanks
016400131129     c                   eval      §cm1mitt = og121
016500131129     c                   when      §mramites = '*OG121' and og121  = *blanks
016600131129     c                   eval      §cm1mitt = 'int' + terminalp
016700131129     c                   other
016800131129     c                   eval      §cm1mitt = %trim(§mramites) + terminalp
016900131129     c                   endsl
017000131129     c*
017100041015     c                   else
017200131129      * se network italia trasmette come INFO +xxx=linea di partenza
017300131129     c                   move      d44po         lnpalfa           3
017400131129     c*
017500131129     c                   select
017600160401     c                   when      §mramit1l = '*OG121' and og121 <> *blanks
017601160401     c                             and §oglpo = '1'
017700131129     c                   eval      §cm1mitt = og121
017800160401     c                   when      §mramit1l = '*OG121' and og121  = *blanks
017801160401     c                             and §oglpo = '1'
017900131129     c                   eval      §cm1mitt = 'info' + lnpalfa
017901160401     c                   when      §mramit2l = '*OG121' and og121 <> *blanks
017902160401     c                             and §oglpo = '2'
017903160401     c                   eval      §cm1mitt = og121
017904160401     c                   when      §mramit2l = '*OG121' and og121  = *blanks
017905160401     c                             and §oglpo = '2'
017906160401     c                   eval      §cm1mitt = 'info' + lnpalfa
017907160413      * inserito questo controllo della sede mancante nella tabella MRA
017908160413     c                   when      §mramit1l = '*OG121' and og121  = *blanks
017909160413     c                             and d44po  = 046
017910160413     c                   eval      §cm1mitt = 'info' + lnpalfa
017911160413     c                   when      §mramit1l = '*OG121' and og121 <> *blanks
017912160413     c                             and d44po  = 046
017913160413     c                   eval      §cm1mitt = og121
018000131129     c                   other
018100041015     c                   if        §oglpo = '1'
018200040929     c                   eval      §cm1mitt = %trim(§mramit1l) + lnpalfa
018300040929     c                   else
018400040929     c                   eval      §cm1mitt = %trim(§mramit2l) + lnpalfa
018500040929     c                   endif
018600131129     c                   endsl
018700131129     c*
018800041015     c                   end
018900041026     c                   eval      d44mit = %trim(§cm1mitt) + mitterr
019000040929     C                   endsr
019100931217     C******************************************************
019200041015     C     esci          BEGSR
019300041015     c                   eval      d44err = '1'
019400041015     c                   seton                                        lr
019500041015     c                   return
019600041015     C                   endsr
019700041015     C******************************************************
019800041015     C     *INZSR        BEGSR
019900931217     C*
020000931217     C     *ENTRY        PLIST
020100041015     C                   PARM                    trul44ds
020200050902     C                   PARM                    dstrtcm1
020300120123     C                   PARM                    key1
020400120123     C                   PARM                    key2
020500041015     c                   clear                   d44dta
020600041015     c                   clear                   d44err
020700041015     c*
020800040629     C     Kntc          KLIST
020900041015     C                   KFLD                    ntcapl
021000041015     C                   KFLD                    ntcnk1
021100041015     C                   KFLD                    ntcnk2
021200041015     C                   KFLD                    ntctnt
021300041015     c                   eval      ntcapl = d44apl
021400041015     c                   eval      ntcnk1 = d44nk1
021500041015     c                   eval      ntcnk2 = d44nk2
021600041015     c                   eval      ntctnt = d44tnt
021700040630     C     ktbe          klist
021800040630     C                   kfld                    tbecod
021900040630     C                   kfld                    tbeke1
022000040630     C                   kfld                    tbeke2
022100041015     c                   move      'MRA'         tbecod
022200120123     c                   move      *blank        tbeke2
022300120123     c                   movel(p)  d44pgm        tbeke1
022400120123     C                   IF        %PARMS=3
022500120123     c                   movel(p)  key1          tbeke1
022600120123     c                   end
022700120123     C                   IF        %PARMS=4
022800120123     c                   movel(p)  key2          tbeke2
022900120123     c                   end
023000040929      * reperimento tabella MRA definizioni invio posta elettronica
023100040929     c                   clear                   dmra
023200040929     c     ktbe          chain     tntbe01l
023300040929     c                   if        %found
023400040929     c                   movel     tbeuni        dmra
023500041015     c                   else
023600041015     c                   exsr      esci
023700040929     c                   endif
023800041102     c                   eval      d44outq = §mraoutqi
023900120119     c* oggi
024000120119     c                   move      udate         dtaiso
024100120119     c                   move      dtaiso        amgdtu            8 0
024200120119     c*
024300041018     C                   if        d44po = *zeros
024400041018     c                   movel     d44ksc        d44po
024500041018     c                   end
024600120119     c* Imposto i dati in LV55 x reperire p.o. che eventualmente mi gestisce
024700120119     c                   clear                   fnlv55ds
024800120119     c                   movel     '6'           d55tpt
024900120119     c                   movel     amgdtu        d55drf
025000120119     c                   movel     d44po         d55lin
025100120119     c                   call      'FNLV55R'
025200120119     c                   parm                    fnlv55ds
025300120119     c                   if        d55tfa <> 0
025400120119     c                   move      d55tfa        d44po
025500120119     c                   end
025600120119      * reperimento organigramma del po mittente
025700041015     c     d44po         chain     azorg01l
025800041015     c                   if        %found
025900041015     c                   movel     orgde8        og148
026000041015     C                   MOVEL     ORGDE3        OG143
026100131129     c                   movel     *blanks       og121            25
026200131129     c                   if        %len(%trim(orgDC1)) > 1
026300131129     c                   movel     orgDC1        og121
026400131129     c                   endif
026500041015     c                   else
026600041015     c                   exsr      esci
026700041015     c                   endif
026800931217     C                   ENDSR
