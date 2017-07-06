      SUBROUTINE INTERPOLATION(A,B,RES,N,AIRE)
*
      INTEGER N,I,K
      DOUBLE PRECISION AIRE, H, RES(0:N),PRIM,RESP,ERR,A,B,FUNC
*
*
      RESP = PRIM(B) - PRIM(A)
      K = 2
      DO WHILE (K.le.N)
         AIRE = 0D0
         H = (B-A) / K
         DO I = 0, K
            RES(I) = FUNC(A + I*H)
         END DO
         AIRE = 5D-1 * (RES(0) + RES(K))
         DO I = 1, K-1
            AIRE = AIRE + RES(I)
         END DO
*
         AIRE = AIRE * H
         ERR = ABS((AIRE - RESP) / RESP)
         ERR = -LOG(ERR) / LOG(2D0)
         WRITE(*,*)'',K,ERR
         K = K + K
      END DO
      WRITE(*,*)''
      WRITE(*,*)'---- Resultat du calcul de la primitive ----'
      WRITE(*,*)'Resultat : ',RESP
      END SUBROUTINE
