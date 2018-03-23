000100081126     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200081126      *------------------------------------------------------------------------*
000300081126      *
000400081126      *                     MANDO MAIL  ?
000500081126      *
000600081126      *------------------------------------------------------------------------*
000700081126     fprtemail  o    f  132        printer  oflind(*inof)  usropn
000800081126      *------------------------------------------------------------------------*
000900081126
001000081126     d kpjba         e ds
001100081126     d tibs02ds      e ds
001200081126
001300081126      * dati per invio mail
001400081126     d lenght          s             15  5
001500081126     d o_testo         s            132    inz
001600081126     d qcmd            s            512    inz
001700081126     d $invio          s              1n   inz(*off)
001800081126     d $okmra          s              1n   inz(*off)
001900081126     d dmradan       e ds                  inz
002000081126     d trtcm1ds      e ds                  inz
002100081126      *    .§cm1mitt = Indirizzo e-mail del mittente
002200081126     d   §cm1mitt    e                     inz('ced@bartolini.it')
002300081126      *    ·§cm1dst  = Indirizzo e-mail del destinatario
002400081126     d   §cm1dst     e                     inz('monica.beghelli@bartolini.it')
002500081126      *    ·§cm1tips = Tipo lettera via e-mail:
002600081126      *     "LET" = testo allegato in corpo con logo
002700081126      *             (richiede righe libere iniziali per il logo)
002800081126      *     "COR" = testo integrato senza logo
002900081126      *             (non consente né UNDERLINE né HIGHLIGHT)
003000081126     d   §cm1tips    e                     inz('COR')
003100081126      *    ·§cm1po   = Filiale
003200081126     d   §cm1po      e                     inz('046')
003300081126      *    ·§cm1var  = Oggetto e-mail
003400081126     d   §cm1var     e                     inz('*OBJM*+
003500081126     d                                     ERRORE')
003600081126      *    ·§cm1sts  = Stato
003700081126     d   §cm1sts     e                     inz(*off)
003800081126      *    ·§cm1idp  = Id processo
003900081126     d   §cm1idp     e                     inz('2')
004000081126
004100081126     d c_sede          c                   const('046')
004200081126      *  - comando di override al prtf
004300081126     d c_cmdovrprtf    c                   const('OVRPRTF +
004400081126     d                                           file(PRTEMAIL) +
004500081126     d                                           pagesize(66 132) +
004600081126     d                                           lpi(6) cpi(10) +
004700081126     d                                           ovrscope(*actgrpdfn) +
004800081126     d                                           ')
004900081126     d c_cmddltovr     c                   const('DLTOVR +
005000081126     d                                            file(PRTEMAIL) +
005100081126     d                                            lvl(*actgrpdfn)')
005200081126
005300081126     c     *entry        plist
005400081126     c                   parm                    kpjba
005500081126
005600081126      * Reperimento tab. "MRA" se non già recuperata
005700081126     c                   if        not $okmra
005800081126     c                   clear                   dmradan
005900081126     c                   clear                   tibs02ds
006000081126     c                   eval      t02mod = 'C'
006100081126     c                   eval      t02sif = knsif
006200081126     c                   eval      t02cod = 'MRA'
006300081126     c                   eval      t02ke1 = 'FIOR90R'
006400081126     c                   call      'TIBS02R'
006500081126     c                   parm                    kpjba
006600081126     c                   parm                    tibs02ds
006700081126     c                   if        t02err = *blanks
006800081126     c                   eval      dmradan = t02uni
006900081126     c                   eval      $okmra = *on
007000081126     c                   endif
007100081126     c                   endif
007200081126
007300081126      * Override al file di stampa ed apertura dello stesso
007400081126     c                   if        not %open(prtemail)
007500081126     c                   exsr      sr_openprtf
007600081126     c                   endif
007700081126
007800081126     c                   eval      o_testo = kpjbu
007900081126     c                   except    prtdet
008000081126
008100081126     c                   exsr      sr_end
008200081126
008300081126     C                   eval      *inlr = *on
008400081126
008500081126      *------------------------------------------------------------------------*
008600081126      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
008700081126      *  + STAMPA INIZIO MAIL
008800081126      *------------------------------------------------------------------------*
008900081126     c     sr_openprtf   begsr
009000081126
009100081126      * Override al file di stampa
009200081126     c                   exsr      sr_override
009300081126
009400081126     c                   open      prtemail
009500081126
009600081126      * Stampa una testata se NON è richiesta la e-mail
009700081126     c                   if        §mradreg = *blanks
009800081126     c                   eval      o_testo = kpjbu
009900081126     c                   except    prtdet
010000081126     c                   eval      o_testo = kcdaz + ' ' + knmus + ' ' + knsif
010100081126     c                   except    prtdet
010200081126     c                   clear                   o_testo
010300081126     c                   except    prtdet
010400081126     c                   endif
010500081126
010600081126      * Stampa testo iniziale
010700081126     c                   eval      o_testo = 'Errore'
010800081126     c                   except    prtdet
010900081126
011000081126      * Stampa una riga vuota
011100081126     c                   clear                   o_testo
011200081126     c                   except    prtdet
011300081126
011400081126     c                   endsr
011500081126
011600081126      *------------------------------------------------------------------------*
011700081126      * OVERRIDE AL FILE DI STAMPA PER IMPOSTARE I DATI PER L'INVIO VIA MAIL
011800081126      *  + STAMPA INIZIO MAIL
011900081126      *------------------------------------------------------------------------*
012000081126     c     sr_override   begsr
012100081126
012200081126     c                   reset                   $invio
012300081126     c                   reset                   trtcm1ds
012400081126
012500081126     c                   if        §mradreg <> *blanks
012600081126     c                   eval      §cm1mitt = %trim(§mradmitt)
012700081126     c                   eval      §cm1dst  = %trim(§mraddest)
012800081126     c                   eval      §cm1tips = §mradreg
012900081126     c                   eval      §cm1po   = c_sede
013000081126     c                   eval      §cm1var  = '*OBJM*' + §mraddes
013100081126     c                   eval      §cm1idp  = §mradidpro
013200081126     c                   eval      qcmd = c_cmdovrprtf
013300081126     c                             + ' outq(' + %trim(§mradoutqi) + ')'
013400081126     c                             + ' usrdfndta(''' + trtcm1ds + ''')'
013500081126     c                   else
013600081126     c                   eval      qcmd = c_cmdovrprtf
013700081126     c                   endif
013800081126
013900081126     c                   eval      lenght = %size(qcmd)
014000081126     c                   call(e)   'QCMDEXC'
014100081126     c                   parm                    qcmd
014200081126     c                   parm                    lenght
014300081126
014400081126     c                   if        §mradreg <> *blanks and not %error
014500081126     c                   eval      $invio = *on
014600081126     c                   endif
014700081126
014800081126     c                   endsr
014900081126
015000081126      *------------------------------------------------------------------------*
015100081126      * ROUTINE FINALE
015200081126      *------------------------------------------------------------------------*
015300081126     c     sr_end        begsr
015400081126
015500081126     c                   if        %open(prtemail)
015600081126
015700081126      * Chiusura dello spool?
015800081126     c                   clear                   o_testo
015900081126     c                   except    prtdet
016000081126     c                   eval      o_testo = '***   Fine Lista   ***'
016100081126     c                   except    prtdet
016200081126
016300081126     c                   close     prtemail
016400081126
016500081126      * Eliminazione overflow?
016600081126     c                   eval      lenght = %size(qcmd)
016700081126     c                   eval      qcmd = c_cmddltovr
016800081126     c                   call(e)   'QCMDEXC'
016900081126     c                   parm                    qcmd
017000081126     c                   parm                    lenght
017100081126
017200081126     c                   endif
017300081126
017400081126     c                   endsr
017500081126
017600081126     oprtemail  e            prtdet      1
017700081126     o                       o_testo
