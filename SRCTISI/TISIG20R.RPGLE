000100150213     h decedit('0,') datedit(*dmy.)
000200150213      * TISIG20R*-----------------------------------------------------*
000300150213      *         - RICEZIONE FIAR5 IN SEDE (BAN - BNB) -               *
000400150213      *               movimenta i Bancali                             *
000500030120      *---------------------------------------------------------------*
000600150220     ffiar531C  If   e           k disk
000700150224     ftntbe01l  If   e           k disk
000800150311     ftabel00f  If   e           k disk
000900150225     fazagb01l  If   e           k disk
001000030120      *---------------------------------------------------------------*
001100150218      * Strutture Dati :
001200030129     d Status         sds           333
001300030129     d  SDSpgm           *proc
001400030120      *
001500030120     d KPJBA         e ds                                                       *architettura
001600150224     d savKPJBU        s                   like(KPJBU)
001700150218      *--------------------------
001800150218     d* Parametri x Controllo profilo utenti
001900150218     d TIBS34ds      e ds
002000150218     d AZUTEds       e ds                  extname(AZUTE00F)
002100150218     d DDatiUte      e ds
002200150224      *--------------------------
002300150224     D dvpoDECO      E DS
002400150311     D dsTB          E DS
002500150311      *--------------------------
002600150224     d kCOD1           s                   like(TBEcod)
002700150224     d kKEY1           s                   like(TBEke1)
002800150218      *--------------------------
002900150218     d Fiar5ds       e ds                  extname(FIAR531C)
003000150218     d Fiar5ds_P     e ds                  extname(FIAR531C) prefix(P_)
003100150220      *--------------------------
003200150218     d TISIG20DS     e ds
003300150224     d TISIGADS      e ds
003400150212     d TISIG0DS      e ds
003500150220      *--------------------------
003600030129     d dAR5ban       e ds                  inz
003700150212     d dAR5bnb       e ds                  inz
003800150220      *--------------------------
003900030120      *
004000030120     d WLBDA8          ds                  inz                                  *ds "XSRDA8"
004100030120     d  G08dat                        8  0 inz                                   -data g/m/aa
004200030120     d  G08inv                        8  0 inz                                   -data aa/m/g
004300030120     d  G08err                        1    inz                                   -errore
004400030120     d  G08tgi                        5  0 inz                                   -n°giorni data
004500030120      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004600030120      * - Campi di comodo
004700150213      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004800030120     d Wn14            s             14  0                                      *numerico 14
004900030120     d Wn08            s              8  0                                      *numerico 8
005000030120     d Woggi           s              8  0                                      *d.corrente aa/m/g
005100030129     d Wora            s              6  0                                      *h.corrente h:m:s
005200030120      *---------------------------------------------------------------*
005300150213     c     *Entry        Plist
005400150218     c                   parm                    KPJBA
005500150218     c                   parm                    Fiar5ds_P
005600150219     c                   eval      TISIG20DS = KPJBU
005700150219      *
005800150219if  1C                   IF        i20tla = *blanks                             R lascia file aperti
005900150219     c                   eval        *inRT = *on
006000150219x   1C                   ELSE                                                   C/L chiude i file
006100150219     c                   eval        *inLR = *on
006200150219      * se deve chiudere
006300150219if  1C                   IF        i20tla = 'C'
006400150219     c                   return
006500150219e   1C                   endIF
006600150219e   1C                   endIF
006700150218      *
006800150218     C                   EVAL      o20ERR = '0'
006900150218     C                   EVAL      o20MSG = *blank
007000150311      *
007100150311      *  Controlla il TIPO BOLLA passato che non deve essere un recupero o C/S
007200150311      *   solo per una Bolla NORMALE
007300150311     C                   z-add     1             tblkut
007400150311     C                   move      'TB'          tblcod
007500150311     C                   movel(p)  i20TBL        tblkey
007600150311     c     ktabel        chain     tabel00f
007700150311     c                   if        %Found(tabel00f)
007800150311     c                   eval      dsTB = tbluni
007900150311     c                   end
008000150311     c                   if        not %Found(tabel00f)
008100150311     c                             or  %Found(tabel00f) and §TBrbl<>'N'
008200150311     C                   EVAL      o20ERR = '1'
008300150311     C                   EVAL      o20MSG =
008400150311     c                             'Il Tipo Bolla: ' + i20TBL +
008500150311     c                             ' Non Movimenta i Bancali.'
008600150311     c                   eval      KPJBU = TISIG20DS
008700150311     c                   RETURN
008800150311     c                   end
008900150224      *
009000150224      * Se non è ancora attivata la procedura alla DATA
009100150224     c                   if        i20DAT < §vpoBANC
009200150224     C                   EVAL      o20ERR = '1'
009300150224     C                   EVAL      o20MSG =
009400150224     c                             'Movimento precedente data ' +
009500150224     c                             %editw(§vpoBANC:'    /  /  ') +
009600150224     c                             ' di attivazione proc.Bancali: '
009700150224     c                   eval      KPJBU = TISIG20DS
009800150224     c                   RETURN
009900150224     c                   end
010000150218      *
010100150311      *
010200150218      *  Se NON ha ricevuto il 2° parametro allora deve agganciare
010300150218      *  il FIAR5 relativo
010400150218     c                   if        %parms > 1
010500150218     c                   eval      fiar5ds = fiar5ds_P
010600150218     c                   else
010700150218      *
010800150218     c     KAR5          chain     fiar531C
010900150218     c                   if        not %Found(fiar531C)
011000150218     C                   EVAL      o20ERR = '1'
011100150218     C                   EVAL      o20MSG =
011200150218     c                              'Non trovato FIAR531C tipo rcd.:' + i20TRD
011300150218     c                   eval      KPJBU = TISIG20DS
011400150218     c                   RETURN
011500150218     c                   end
011600150218     c                   end
011700150224      *
011800150224      * -------------------------              -------------------------
011900150224      * decide se eseguire il MOVIMENTO
012000150224      * -------------------------              -------------------------
012100150224      *
012200150224     c                   clear                   esegui_MOV        1
012300150224      *
012400150224      *  in funzione del tipo decodifico con la sua DS.
012500150224if  2c                   IF         i20trd = 'BNB'
012600150224     c                   movel     ar5uni        dAR5bnb
012700150224     c                   eval       esegui_MOV ='S'
012800150224     c                   endIF
012900150224      *
013000150224      *  in funzione del tipo decodifico con la sua DS.
013100150224if  2c                   if         i20trd = 'BAN'
013200150224     c                   movel     ar5uni        dAR5ban
013300150225     c                   clear                   tisigads
013400150226     c                   clear                   Esegue_BAN1       1
013500150226     c                   clear                   Esegue_BAN2       1
013600150224      *
013700150224      * controlla il cod.cliente  mediante il Driver
013800150225     c                   if        §AR5tba > 0
013900150224     c                   move      i20sog        IGAKSC
014000150224     c                   move      i20dat        IGADAT
014100150224     c                   eval      IGACOD = 'B' + %editc(§AR5tba:'X')
014200150226     c                   eval      IGATSC = 'B'
014300150224     c                   eval      savKPJBU = KPJBU
014400150224     c                   eval       kpjbu = tisigads
014500150224     c                   call      'TISIGAR'
014600150224     c                   parm                    kpjba
014700150224     c                   eval       tisigads = kpjbu
014800150224     c                   eval      KPJBU = savKPJBU
014900150226     c                   if        ogaKSU <> *blank and ogaESITO= *blank
015000150226     c                   eval         Esegue_BAN1 = 'S'
015100150226     c                   end
015200150225     c                   end
015300150224      *
015400150224      * Se con il primo articolo non ha dato buon Esito
015500150224      *  allora prova con il secondo
015600150225     c                   if        §AR5tb2 > 0
015700150224     c                   clear                   tisigads
015800150224     c                   move      i20sog        IGAKSC
015900150224     c                   move      i20dat        IGADAT
016000150224     c                   eval      IGACOD = 'B' + %editc(§AR5tb2:'X')
016100150226     c                   eval      IGATSC = 'B'
016200150224     c                   eval      savKPJBU = KPJBU
016300150224     c                   eval       kpjbu = tisigads
016400150224     c                   call      'TISIGAR'
016500150224     c                   parm                    kpjba
016600150224     c                   eval       tisigads = kpjbu
016700150224     c                   eval      KPJBU = savKPJBU
016800150226     c                   if        ogaKSU <> *blank and ogaESITO= *blank
016900150226     c                   eval         Esegue_BAN2 = 'S'
017000150226     c                   end
017100150225     c                   end
017200150224      *
017300150224      *   se c'è il codice AUT e l'esito è OK
017400150226     c                   if           Esegue_BAN1 = 'S' or
017500150226     c                                Esegue_BAN2 = 'S'
017600150224      *
017700150224     c                   eval       esegui_MOV ='S'
017800150224      *
017900150224     c                   else
018000150224     C                   EVAL      o20ERR = '1'
018100150224     C                   EVAL      o20MSG = ogaTESTO
018200150218     c                   end
018300150224      *
018400150224     c                   endIF
018500150213      *
018600150218      *   x la gestione dei Bancali  allora deve scrivere il movimento
018700150213      * registrazione movimenti Bancali
018800150224     c                   if         esegui_MOV ='S'
018900150218     c                   exsr      mov_Bancali
019000150213e   1c                   endif
019100150213      *
019200150218     c                   eval       KPJBU = TISIG20DS
019300150213e   2c                   RETURN
019400150212     C*--------------------------------------------------------------------------------------------*
019500150212     C* considerazioni per gestione Bancali nelle spedizioni
019600150212     C*--------------------------------------------------------------------------------------------*
019700150218     C     mov_Bancali   BEGSR
019800150212      *
019900150212      * scrive il record Movimento dei bancali
020000150212     C                   CLEAR                   TISIG0DS
020100150212      *
020200150212      *  x  Tipo Bancale: BAN
020300150220if  2c                   if               i20trd = 'BAN'
020400150224     C                   MOVEL     ogaKSU        IG0SOG
020500150218      *
020600150212     C                   MOVEL     'RTMA'        IG0CAU
020700150218      *  1° bancale BAN
020800150226if  1c                   if        §AR5nba > *zeros and
020900150226     c                                Esegue_BAN1 = 'S'
021000150218     c                   eval      IG0COD = 'B' + %editc(§AR5tba:'X')
021100150212     C                   Z-ADD     §AR5nba       IG0QTA
021200150212     C                   EXSR      scrpfmov
021300150212e   1c                   endif
021400150218      *  2° bancale BAN
021500150226if  1c                   if        §AR5nb2 > *zeros and
021600150226     c                                Esegue_BAN2 = 'S'
021700150218     c                   eval      IG0COD = 'B' + %editc(§AR5tb2:'X')
021800150212     C                   Z-ADD     §AR5nb2       IG0QTA
021900150212     C                   EXSR      scrpfmov
022000150212e   1c                   endif
022100150212      *
022200150212      *  x  Tipo Bancale: BNB
022300150220if  2c                   elseIf           i20trd = 'BNB'
022400150218      *  solo la filiale
022500150219     C                   move      *all'0'       IG0SOG
022600150219     C                   move      i20LNP        IG0SOG
022700150218      *
022800150219     C                   MOVEL     'FIU '        IG0CAU
022900150218      *  1° bancale BNB
023000150212if  1c                   if        §AR5Bnba > *zeros
023100150218     c                   eval      IG0COD = 'O' + %editc(§AR5Btba:'X')
023200150224      *
023300150225      * se ha la serie l'articolo deve avere una "L"
023400150224      *   questo per distinguere se la nostra logistica ha già imbancalato
023500150224     C                   if        i20NRS > 0
023600150225     c                   eval      IG0COD = %trim(IG0COD) + 'L'
023700150224e   1c                   endif
023800150224      *
023900150212     C                   Z-ADD     §AR5Bnba      IG0QTA
024000150225     C                   EXSR      chk_articolo
024100150225     c                   if        trovato_COD = 'S'
024200150212     C                   EXSR      scrpfmov
024300150225e   1c                   end
024400150212e   1c                   endif
024500150218      *  2° bancale BNB
024600150212if  1c                   if        §AR5Bnb2 > *zeros
024700150218     c                   eval      IG0COD = 'O' + %editc(§AR5Btb2:'X')
024800150224      *
024900150225      * se ha la serie l'articolo deve avere una "L"
025000150224      *   questo per distinguere se la nostra logistica ha già imbancalato
025100150224     C                   if        i20NRS > 0
025200150225     c                   eval      IG0COD = %trim(IG0COD) + 'L'
025300150224e   1c                   endif
025400150224      *
025500150212     C                   Z-ADD     §AR5Bnb2      IG0QTA
025600150225     C                   EXSR      chk_articolo
025700150225     c                   if        trovato_COD = 'S'
025800150225     C                   EXSR      scrpfmov
025900150225e   1c                   end
026000150212e   1c                   endif
026100150212      *
026200150212     c                   end
026300150212      *
026400150212     c                   ENDSR
026500150225     **--------------------------------------------------------------------------------------------*
026600150225      * SOLO x il BNB prima di scrivere il movimento controlla l'ARTICOLO
026700150225      *      x il BAN lo ha già fatto il driver TISIGAR.
026800150225     **--------------------------------------------------------------------------------------------*
026900150225     C     chk_Articolo  BEGSR
027000150225      *
027100150225      * x ultimo e x sicurezza
027200150225      *  controlla esistenza Articolo in Anagrafica proprio prima di scrivere
027300150225      *   il movimento.
027400150225     c                   eval      trovato_COD = 'S'
027500150225     c     IG0COD        chain     azagb01l
027600150225     c                   if        not %Found
027700150225     c                   clear                   trovato_COD       1
027800150225      *
027900150225      *  è solo un ALERT non un vero e proprio errore che fa uscire dal programma,
028000150225      *    essendo 2 i bancali sulle DS, va avanti.
028100150225     C                   EVAL      o20ERR = '1'
028200150225     C                   EVAL      o20MSG =
028300150225     c                              'Non esiste l''articolo :' +
028400150225     c                              %trim(ig0COD) + ' in anagrafica.'
028500150225     c                   end
028600150225      *
028700150225     c                   ENDSR
028800150225     C*--------------------------------------------------------------------------------------------*
028900150225     C* scrpfMOV - scrive il record fisico
029000150225     C*--------------------------------------------------------------------------------------------*
029100150225     C     scrpfmov      BEGSR
029200150225      *
029300150212     C* Imposta la DS di procedura da passare al "driver" x la scrittura del movimento
029400150220     C                   Z-ADD     i20DAT        IG0DAT
029500150212     C                   Z-ADD     wORA          IG0ORA
029600150218     C                   MOVEL     DUTute        IG0UTE
029700150218     C                   Z-ADD     DUTpou        IG0POU
029800150212     C                   movel     SDSpgm        IG0PGM
029900150330      **
030000150330      * Solo x la causale "RTMA" il POU deve essere uguale alla LNP della sped.
030100150330     C                   if          IG0CAU = 'RTMA'
030200150330     C                   Z-ADD     I20LNP        IG0POU
030300150330     C                   end
030400150218      *
030500150227     c                   eval      savKPJBU = KPJBU
030600150212     C* Lancio del "driver"
030700150212     C                   CALL(e)   'TISIG5R'
030800150227     C                   PARM                    kpjba
030900150227     C                   PARM                    TISIG0DS
031000150227     c                   eval      KPJBU = savKPJBU
031100150212     C*
031200150212     C* Se errore in chiamata "driver" gestisco come se restituito errore dal "driver" stesso
031300150218     C                   IF        %error or OG0ERR ='E'
031400150218      *
031500150218     C                   EVAL      o20ERR = '1'
031600150218     C                   EVAL      o20MSG = OG0MSG
031700150218     c                   eval      KPJBU = TISIG20DS
031800150218     c                   RETURN
031900150218      *
032000150212     C                   ENDIF
032100150212     C*
032200150212     C                   ENDSR
032300150213      **-------------------------------------------------------------**
032400150213      ** Operazioni iniziali                                         **
032500150213      **-------------------------------------------------------------**
032600150213     c     *InzSR        BEGSR
032700150213      *
032800150218     c     KAR5          klist
032900150218     c                   kfld                    i20aas
033000150218     c                   kfld                    i20lnp
033100150218     c                   kfld                    i20nrs
033200150218     c                   kfld                    i20nsp
033300150218     c                   kfld                    i20trd
033400150224      *
033500150224     C     KTBE          KLIST
033600150224     C                   KFLD                    KCOD1
033700150224     C                   KFLD                    KKEY1
033800150311      *
033900150311     C     KTabel        KLIST
034000150311     C                   KFLD                    tblkut
034100150311     C                   KFLD                    tblcod
034200150311     C                   KFLD                    tblkey
034300150219      *
034400150219      * Reperisco dati job
034500150219     c                   exsr      DatiJob
034600150224     C*
034700150224     C*  Aggancio tabella 'VPO' x reperire LIMITI CONTROLLO DISTINTA
034800150224     C                   clear                   DvpoDECO
034900150224     C                   MOVEL     'VPO'         KCOD1
035000150224     C                   MOVEL     *BLANKS       KKEY1
035100150224     C                   MOVEL     'DECO'        KKEY1
035200150224     C     KTBE          CHAIN     TNTBE01L
035300150224     C                   If        %Found(TNTBE01L)
035400150224     C                   MOVEL     TBEUNI        DvpoDECO
035500150224     C                   END
035600150220      *
035700150219      * imposta la data corrente
035800150219     c                   time                    Wn14                           *ora(6) + data(8)
035900150219     c                   movel     Wn14          Wora                           *ora(6)
036000150219     c                   move      Wn14          Wn08                           *data(8) in g/m/aa
036100150219     c                   z-add     Wn08          G08dat
036200150219     c                   clear                   G08inv
036300150219     c                   movel     *off          G08err
036400150219     c                   call      'XSRDA8'
036500150219     c                   parm                    WLBDA8
036600150219     c                   z-add     G08inv        Woggi                          *d.corrente aa/m/g
036700150218      *
036800150213     c                   ENDSR
036900150218      *---------------------------------------------------------------*
037000150218      *?  Reperimento Dati del job (Utente/Operativi)                ?
037100150218      *---------------------------------------------------------------*
037200150218     c     DatiJOB       BEGSR
037300150218      *
037400150218     c     *dtaara       define    §azute        azuteds
037500150218     c     *dtaara       define    §datiute      ddatiute
037600150218      *
037700150218     c                   in(E)     *dtaara
037800150218     c                   IF        %ERROR or RSUT = *blanks
037900150218     c                   clear                   Tibs34Ds
038000150218     c                   call      'TIBS34R'
038100150218     c                   parm                    Tibs34Ds
038200150218     c                   in        *dtaara
038300150218     c                   ENDIF
038400150218      *
038500150218     c                   ENDSR
038600150218     C*---------------------------------------------------------------*
