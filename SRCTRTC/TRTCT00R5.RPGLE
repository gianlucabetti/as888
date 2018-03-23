000100121224     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200090202      *-------------------------------------------------------------------------------
000300110202      * SUDDIVIDE in singoli campi una stringa contenente tutti gli indirizzi MAIL
000400110202      *  e restituisce max 10 indirizzi a cui spedire un messaggio.
000500090202      *-------------------------------------------------------------------------------
000600110621     d  esito          s              1
000700110202      *  Invio E-mail
000800110202     D    wrkEml       s            253
000900130204     D    wrkEmlcc     s            253
001000110202     D    wrkOgg       s             44
001100110202     D    wrkMsg       s           5000
001200130204     D    wrkEsito     s              1
001300110202      *
001400110202      * ?Definizioni x gestire lo split del record passato
001500110202     D  Lun_dati       s              5i 0 INZ(0)
001600110202     D in_X_prima      s              5i 0 INZ(0)
001700110202     D  Dati           s            253
001800110202     D separatore      s              1
001900110202     D Campo           s            253    INZ(' ')
002000110202      *
002100110202      *
002200110202     D  in_X           s              5i 0 INZ(0)
002300110202     D  da_X           s              5i 0 INZ(0)
002400110202     D Campo_Nr        s              3u 0 INZ(0)
002500110202     D Campo_Lungo     s              3u 0 INZ(0)
002600110202      *
002700050713      * ?------------------------------------------------------------------ */
002800050629     D Digits          C                   CONST('0123456789')
002900050628      * ?------------------------------------------------------------------ */
003000110202      * ?     Input
003100050628     c     *entry        plist
003200110208     C                   parm                    Indirizzi       253
003300130204     C                   parm                    Conoscenza      253
003400110208     C                   parm                    Oggetto          44
003500110208     C                   parm                    Messaggio      5000
003600130204     C                   parm                    wrkesito          1
003700110208      *
003800110208     c                   z-add     0             Campo_Nr
003900110202      *
004000110202      **  Controlla dove trovare il separatore
004100110202      **  e si deve partire la prima volta dalla prima posizione
004200110202      **  impostando la lunghezza massima della stringa di carattere passata
004300110202      **  che compone il record. (Questo per non perdere tempo dopo l'ultimo
004400110202      **  carattere valido con dei blanks inutili)
004500110202      *
004600110202     c                   eval      in_X_prima = 1
004700110202     c                   eval      separatore = ';'
004800110202     c                   eval      Dati       = Indirizzi
004900110202     c                   eval      Lun_dati = %Len(%TrimR(Dati))
005000110202      *
005100110202      * esegue un controllo su l'ultimo carattere della stringa passata e se non c'è il
005200110202      *  separatore che ci si aspetta lo aggiunge per poter seguire lo split anche
005300110202      *  dell'ultimo campo.
005400110202     c                   if        %subst(Dati:Lun_dati:1) <> separatore
005500110202     c                   eval      Lun_dati = Lun_dati + 1
005600110202     c                   eval      %subst(Dati:Lun_dati:1) = separatore
005700110202     c                   end
005800110202      **
005900110202      **  Esegue un loop byte x byte fino alla lunghezza massima
006000110202      **  per cercare dove si trova il separatore
006100110202     c                   do        Lun_dati      in_X
006200110202      **
006300110202      * ?   ( se trovato separatore imposta il nuovo campo )
006400110202      * ?     dal punto in cui si trova il primo byte non separatore fino al primo
006500110202      * ?     separatore trovato.
006600110202     c                   If        %Subst(Dati:in_X:1) = separatore
006700110202      **
006800110202      **      si memorizza il nr.campo che sta elaborando e resetta i flags
006900110202     c                   add       1             Campo_Nr
007000110202      **
007100110202      **  Imposta i campi
007200110202      **   occorre impostare da quale byte iniziare della stringa da splittare
007300110202      **   per poi eseguire la SUBSTRING e recepire l'informazione contenuta.
007400110202     c                   if        Campo_Nr = 1
007500110202      * ?  Se è il primo campo è il punto da cui partire quindi è il 1 byte
007600110202     c                   eval      da_X = 1
007700110202     c                   else
007800110202      * ?   punto da cui partire successivamente
007900110202     c                   eval      da_X = in_X_prima + 1
008000110202     c                   end
008100110202      **
008200110202      **  Lunghezza del contenuto è il punto in cui si è trovato il separatore
008300110202      **   meno il byte da dove inizia l'informazione.
008400110202     c                   eval      Campo_Lungo = in_X - da_X
008500110202      * ?   ( Campo Vuoto )
008600110202      **   se la differenza è zero allora il campo è vuoto ossia ci sono
008700110202      **   2 separatori contigui.
008800110202     c                   if        Campo_Lungo > 0
008900110202      * ?   ( imposta il dato rilevato nel campo )
009000110202      *  quindi per SUBSTRING rileva l'informazione e la memorizza sul CAMPO generico
009100110202     c                   eval      campo = %subst(Dati:da_X:Campo_Lungo)
009200110202      *
009300110202     c                   exsr      INVIA_MAIL
009400110202     c                   end
009500110202      **
009600110202      ** Uscendo dal ciclo
009700110202      ** Memorizza la posizione del separatore per reiniziare un nuovo
009800110202      *   ciclo.
009900110202     c                   eval      in_X_prima = in_X
010000110202     c                   EndIf
010100110202
010200110202     c                   EndDo
010300110202
010400110202     c                   seton                                        RT
010500110202      *--------------------------------------------------------------------
010600110202      *   INVIA MAIL
010700110202      *____________________________________________________________________
010800110202     c     INVIA_MAIL    BEGSR
010900110202      *
011000110202     C* Inizializzo variabili
011100110202     C                   eval      wrkEml = Campo
011200110202     C                   eval      wrkOgg = Oggetto
011300110202     C                   eval      wrkMsg = Messaggio
011400110202      *
011500130204     C                   eval         wrkesito = *blank
011600130204     C                   eval         wrkEmlCC = *blank
011700130204      *
011800130204     C                   call(e)   'TIS701C1'
011900110202     C                   parm                    wrkEml
012000130204     C                   parm                    wrkEmlcc
012100110202     C                   parm                    wrkOgg
012200110202     C                   parm                    wrkMsg
012300130204     C                   parm                    wrkesito
012400110202      *
012500110202     c                   ENDSR
012600050628      * ?------------------------------------------------------------------ */
012700110621      * ?------------------------------------------------------------------ */
012800110621      *?      X non bloccare in nessun caso il traduttore CLIENTI
012900110621      * ?------------------------------------------------------------------ */
013000110621     C     *pssr         BEGSR
013100110621     C
013200110621     C                   eval      esito = 'E'
013300110621     C                   ENDSR     '*CANCL'
013400110621     C
013500110621      *  ------------------------------------------------------------------ */
