000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020529      *
000300080603      * TRULDEsR*---------------------------------------------------*
000400080603      * - Verifica se destinatario conosciuto
000500080604      * i campi in input vengono definiti in questo modo se si possiede
000600080604      * già l'informazione completa del destinatario si possono passare
000700080604      * senza i riferimenti della spedizione.
000800080604      * altrimenti viene passata la spedizione che
000900080604      * indica di effettuare prima una chain sulla spedizine che si sta gestendo
001000080604      * per ricavare le informazioni sul destinatario
001100080604      *
001200080604      * INPUT
001300080604      * la ds di parametri contiene sia i campi standard della spedizione
001400080604      * che i campi del destinatario
001500080604      * OUTPUT
001600080604      * esito
001700080604      * riferimenti spedizione di riferimento
001800060920      *--------------------------------------------------------------*
001900020412
002000080604     ffnarb01l  if   e           k disk
002100080603     ffnblp09l  if   e           k disk
002200020412
002300080603     d truldesds     e ds
002400020529
002500020412      *---------------------------------------------------------------
002600080604     c     karb          klist
002700080604     c                   kfld                    idsaas
002800080604     c                   kfld                    idslnp
002900080604     c                   kfld                    idsnrs
003000080604     c                   kfld                    idsnsp
003100080604     c*
003200080604     c     kblp          klist
003300080604     c                   kfld                    idsRSD
003400080604     c                   kfld                    idsIND
003500080604     c                   kfld                    idsLOD
003600080604     c                   kfld                    idsPRD
003700080604     c                   kfld                    idsNZD
003800020412     c     *entry        plist
003900080603     c                   parm                    truldesds
004000080604      *
004100080604     c                   if        idsnsp > 0
004200080604     c     karb          chain     fnarb01l
004300080604     c                   if        %found(fnarb01l)
004400080604     c                   move      arbRSD        idsRSD
004500080604     c                   move      arbIND        idsIND
004600080604     c                   move      arbLOD        idsLOD
004700080604     c                   move      arbPRD        idsPRD
004800080604     c                   move      arbNZD        idsNZD
004900080604     c                   else
005000080604     c                   return
005100080604     c                   endif
005200080604     c                   endif
005300020529
005400080603     c                   eval      odsesito = *blanks
005500080604     c                   eval      odsbolla = *blanks
005600020412
005700020412     c                   eval      *inlr = *on
005800080603     c     kblp          setll     fnblp09l
005900060921     c                   do        *hival
006000080603     c     kblp          reade     fnblp09l
006100080603     c                   if        %eof(fnblp09l)
006200060921     c                   leave
006300060921     c                   endif
006400080603     c                   if        blpdcm > 0 and blpcca = ' '
006500080603     c                   eval      odsesito = 'S'
006600080604     c                   eval      odsbolla =  %editc(blplnp:'Z') +  ' '
006700080604     c                             + %editc(blpnrs:'Z') +  ' '
006800080604     c                             + %editc(blpnsp:'Z')
006900080603     c                   leave
007000060921     c                   endif
007100060921     c                   enddo
007200060920     c
007300020412     c                   return
007400080603      *
