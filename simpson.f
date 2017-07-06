      SUBROUTINE INTERPOLATION(A,B,RES,N,AIRE)
*
      INTEGER N,I,K
      DOUBLE PRECISION H, RES(0:N),AIRE, PRIM,RESP,ERR,A,B,FUNC
*
      RESP = PRIM(B) - PRIM(A)
      K = 2
      DO WHILE (K.le.N)
         AIRE = 0D0
         H = (B-A) / K
         DO I = 0, K
            RES(I) = FUNC(A + I*H)
         END DO
*
         AIRE = 0D0
*
* DE RES(1) a RES(N-2)
         DO I = 1, K/2
            AIRE = AIRE + 4 * RES(2*I - 1) + 2 * RES(2*I)
         END DO
         AIRE = AIRE + RES(0) - RES(K)
         AIRE = H * AIRE/3
*        WRITE(*,*) 'AIRE',AIRE

* approximation du nombre de bits exacts        
         ERR = ABS((AIRE - RESP)/RESP)
         ERR = -LOG(ERR)/LOG(2D0)
         WRITE(*,*)'',K,ERR
         K = K + K
      END DO
      WRITE(*,*)''
      WRITE(*,*)'---- Resultat du calcul de la primitive ----'
      WRITE(*,*)'Resultat : ',RESP
      END SUBROUTINE
          
