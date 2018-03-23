000100150213     h decedit('0,') datedit(*dmy.)
000200150213      * TISIG20R*-----------------------------------------------------*
000300150213      *         - RICEZIONE FIAR5 IN SEDE (BAN - BNB) -               *
000400150213      *               movimenta i Bancali                             *
000500030120      *---------------------------------------------------------------*
000600030120     fTITAS30C  If   e           k disk
000700150212     fazFGB02L  If   e           k disk
000800150218     ffiar531C  If   e           k disk
000900030120      *---------------------------------------------------------------*
001000150218      * Strutture Dati :
001100030129     d Status         sds           333
001200030129     d  SDSpgm           *proc
001300030120      *
001400030120     d KPJBA         e ds                                                       *architettura
001500030129      *
001600150218      *--------------------------
001700150218     d* Parametri x Controllo profilo utenti
001800150218     d TIBS34ds      e ds
001900150218      *--------------------------
002000150218     d AZUTEds       e ds                  extname(AZUTE00F)
002100150218     d DDatiUte      e ds
002200150218      *--------------------------
002300150218     d Fiar5ds       e ds                  extname(FIAR531C)
002400150218     d Fiar5ds_P     e ds                  extname(FIAR531C) prefix(P_)
002500150218     d TISIG20DS     e ds
002600150212     d TISIG0DS      e ds
002700030129     d dAR5ban       e ds                  inz
002800150212     d dAR5bnb       e ds                  inz
002900030120      *
003000030120     d WLBDA8          ds                  inz                                  *ds "XSRDA8"
003100030120     d  G08dat                        8  0 inz                                   -data g/m/aa
003200030120     d  G08inv                        8  0 inz                                   -data aa/m/g
003300030120     d  G08err                        1    inz                                   -errore
003400030120     d  G08tgi                        5  0 inz                                   -n°giorni data
003500030130      *
003600030130     d dsRIFDOC        ds            14    inz
003700030130     d  TASlnp
003800150213     d    Wsep1                       1    inz('-')
003900030130     d  TASnrs
004000150213     d    Wsep2                       1    inz('-')
004100030130     d  TASnsp
004200030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004300030120      * - Campi di comodo
004400150213      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004500030120     d Wn14            s             14  0                                      *numerico 14
004600030120     d Wn08            s              8  0                                      *numerico 8
004700030120     d Woggi           s              8  0                                      *d.corrente aa/m/g
004800030129     d Wora            s              6  0                                      *h.corrente h:m:s
004900030120      *---------------------------------------------------------------*
005000030120     iTITAS000      20
005100030120     iTITAS010      21
005200030120     iTITASP00      22
005300030120      *---------------------------------------------------------------*
005400150213     c     *Entry        Plist
005500150218     c                   parm                    KPJBA
005600150218     c                   parm                    Fiar5ds_P
005700150218     c                   eval      *inLR = *on
005800150218      *
005900150218     c                   eval      TISIG20DS = KPJBU
006000150218     C                   EVAL      o20ERR = '0'
006100150218     C                   EVAL      o20MSG = *blank
006200150218      * Reperisco dati job
006300150218     c                   exsr      DatiJob
006400150218      *
006500150218      *  Se NON ha ricevuto il 2° parametro allora deve agganciare
006600150218      *  il FIAR5 relativo
006700150218     c                   if        %parms > 1
006800150218     c                   eval      fiar5ds = fiar5ds_P
006900150218     c                   else
007000150218      *
007100150218     c     KAR5          chain     fiar531C
007200150218     c                   if        not %Found(fiar531C)
007300150218     C                   EVAL      o20ERR = '1'
007400150218     C                   EVAL      o20MSG =
007500150218     c                              'Non trovato FIAR531C tipo rcd.:' + i20TRD
007600150218     c                   eval      KPJBU = TISIG20DS
007700150218     c                   RETURN
007800150218     c                   end
007900150218     c                   end
008000150218      *
008100150218      * imposta la data corrente
008200150213     c                   time                    Wn14                           *ora(6) + data(8)
008300150213     c                   movel     Wn14          Wora                           *ora(6)
008400150213     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
008500150213     c                   z-add     Wn08          G08dat
008600150213     c                   clear                   G08inv
008700150213     c                   movel     *off          G08err
008800150213     c                   call      'XSRDA8'
008900150213     c                   parm                    WLBDA8
009000150213     c                   z-add     G08inv        Woggi                          *d.corrente aa/m/g
009100150213      *
009200150218      * controlla se esiste la bolla
009300150213     c                   setoff                                       202122
009400150213     c     KBolla        chain     TITAS30C
009500150218     c                   if        not %Found(TITAS30C)
009600150218      *
009700150218     C                   EVAL      o20ERR = '1'
009800150218     C                   EVAL      o20MSG =
009900150218     c                              'Non trovata la bolla su TITAS30C !!!'
010000150218     c                   eval      KPJBU = TISIG20DS
010100150218     c                   RETURN
010200150218      *
010300150218     c                   else
010400150213      *
010500150213      * imposta il Soggetto
010600150213      *   se presente in anagrafica Bancali
010700150213     c                   move      *zero         codFiglio         8
010800150213      *
010900150213w   1c                   if        TASccm <> *zeros
011000150213     c                   move      TASccm        codFiglio
011100150213w   1c                   else
011200150213     c                   move      TASksc        codFiglio
011300150213e   1c                   end
011400150213      *
011500150213     c     codFiglio     chain     azFGB02L
011600150218      *
011700150218      *   se "BAN" e il cliente è iscritto nell'anagrafica
011800150218      *   oppure è un "BNB"
011900150218     c                   if        %Found(azFGB02L) and
012000150218     c                             fgbANN =' ' and ar5TRD ='BAN'
012100150218     c                             or
012200150218     c                             ar5TRD ='BNB'
012300150218      *
012400150218      *  in funzione del tipo decodifico con la sua DS.
012500150218if  2c                   if         ar5trd = 'BAN'
012600150218     c                   movel     ar5uni        dAR5ban
012700150218     c**
012800150218if  2c                   elseif     ar5trd = 'BNB'
012900150218     c                   movel     ar5uni        dAR5bnb
013000150218     c                   end
013100150213      *
013200150218      *   x la gestione dei Bancali  allora deve scrivere il movimento
013300150213      * registrazione movimenti Bancali
013400150218     c                   exsr      mov_Bancali
013500150213      *
013600150213e   1c                   endif
013700150213e   2c                   endif
013800150213      *
013900150218     c                   eval       KPJBU = TISIG20DS
014000150213e   2c                   RETURN
014100150212     C*--------------------------------------------------------------------------------------------*
014200150212     C* considerazioni per gestione Bancali nelle spedizioni
014300150212     C*--------------------------------------------------------------------------------------------*
014400150218     C     mov_Bancali   BEGSR
014500150212      *
014600150212      * scrive il record Movimento dei bancali
014700150212     C                   CLEAR                   TISIG0DS
014800150212      *
014900150212      *  x  Tipo Bancale: BAN
015000150218if  2c                   if               ar5trd = 'BAN'
015100150218     C                   MOVEL     fgbKSU        IG0SOG
015200150218      *
015300150212     C                   MOVEL     'RTMA'        IG0CAU
015400150212      *  se però è un RESO la causale è :
015500150212     c                   if        tasFBR ='S'
015600150212     C                   MOVEL     'REME'        IG0CAU
015700150212     c                   end
015800150218      *  1° bancale BAN
015900150212if  1c                   if        §AR5nba > *zeros
016000150218     c                   eval      IG0COD = 'B' + %editc(§AR5tba:'X')
016100150212     C                   Z-ADD     §AR5nba       IG0QTA
016200150212     C                   EXSR      scrpfmov
016300150212e   1c                   endif
016400150218      *  2° bancale BAN
016500150212if  1c                   if        §AR5nb2 > *zeros
016600150218     c                   eval      IG0COD = 'B' + %editc(§AR5tb2:'X')
016700150212     C                   Z-ADD     §AR5nb2       IG0QTA
016800150212     C                   EXSR      scrpfmov
016900150212e   1c                   endif
017000150212      *
017100150212      *  x  Tipo Bancale: BNB
017200150218if  2c                   elseIf           ar5trd = 'BNB'
017300150218      *  solo la filiale
017400150219     C                   move      *all'0'       IG0SOG
017500150219     C                   move      i20LNP        IG0SOG
017600150218      *
017700150219     C                   MOVEL     'FIU '        IG0CAU
017800150218      *  1° bancale BNB
017900150212if  1c                   if        §AR5Bnba > *zeros
018000150218     c                   eval      IG0COD = 'O' + %editc(§AR5Btba:'X')
018100150212     C                   Z-ADD     §AR5Bnba      IG0QTA
018200150212     C                   EXSR      scrpfmov
018300150212e   1c                   endif
018400150218      *  2° bancale BNB
018500150212if  1c                   if        §AR5Bnb2 > *zeros
018600150218     c                   eval      IG0COD = 'O' + %editc(§AR5Btb2:'X')
018700150212     C                   Z-ADD     §AR5Bnb2      IG0QTA
018800150212     C                   EXSR      scrpfmov
018900150212e   1c                   endif
019000150212      *
019100150212     c                   end
019200150212      *
019300150212     c                   ENDSR
019400150212     C*--------------------------------------------------------------------------------------------*
019500150212     C* scrpfMOV - scrive il record fisico
019600150212     C*--------------------------------------------------------------------------------------------*
019700150212     C     scrpfmov      BEGSR
019800150212     C*
019900150212     C* Imposta la DS di procedura da passare al "driver" x la scrittura del movimento
020000150212     C                   Z-ADD     wOGGI         IG0DAT
020100150212     C                   Z-ADD     wORA          IG0ORA
020200150218     C                   MOVEL     DUTute        IG0UTE
020300150218     C                   Z-ADD     DUTpou        IG0POU
020400150212     C                   movel     SDSpgm        IG0PGM
020500150218      *
020600150212     C                   eval      IG0RDO = dsRIFDOC
020700150212     C                   eval      IG0DDO = (TASaas * 10000) + TASmgs
020800150212     C* Lancio del "driver"
020900150212     C                   CALL(e)   'TISIG5R'
021000150212     C                   PARM                    TISIG0DS
021100150212     C*
021200150212     C* Se errore in chiamata "driver" gestisco come se restituito errore dal "driver" stesso
021300150218     C                   IF        %error or OG0ERR ='E'
021400150218      *
021500150218     C                   EVAL      o20ERR = '1'
021600150218     C                   EVAL      o20MSG = OG0MSG
021700150218     c                   eval      KPJBU = TISIG20DS
021800150218     c                   RETURN
021900150218      *
022000150212     C                   ENDIF
022100150212     C*
022200150212     C                   ENDSR
022300150213      **-------------------------------------------------------------**
022400150213      ** Operazioni iniziali                                         **
022500150213      **-------------------------------------------------------------**
022600150213     c     *InzSR        BEGSR
022700150213
022800150213      * - TITAS30C > INcompleta (4 campi chiave)
022900150213     c     KBolla        klist
023000150218     c                   kfld                    i20AAS
023100150218     c                   kfld                    i20LNP
023200150218     c                   kfld                    i20NRS
023300150218     c                   kfld                    i20NSP
023400150213      *
023500150218     c     KAR5          klist
023600150218     c                   kfld                    i20aas
023700150218     c                   kfld                    i20lnp
023800150218     c                   kfld                    i20nrs
023900150218     c                   kfld                    i20nsp
024000150218     c                   kfld                    i20trd
024100150218      *
024200150213     c                   ENDSR
024300150218      *---------------------------------------------------------------*
024400150218      *?  Reperimento Dati del job (Utente/Operativi)                ?
024500150218      *---------------------------------------------------------------*
024600150218     c     DatiJOB       BEGSR
024700150218      *
024800150218     c     *dtaara       define    §azute        azuteds
024900150218     c     *dtaara       define    §datiute      ddatiute
025000150218      *
025100150218     c                   in(E)     *dtaara
025200150218     c                   IF        %ERROR or RSUT = *blanks
025300150218     c                   clear                   Tibs34Ds
025400150218     c                   call      'TIBS34R'
025500150218     c                   parm                    Tibs34Ds
025600150218     c                   in        *dtaara
025700150218     c                   ENDIF
025800150218      *
025900150218     c                   ENDSR
026000150218     C*---------------------------------------------------------------*
