# CollisionDetector

A smart tool that helps you track DOM element collisions in Elm and trigger updates in model and side effects.

- This is not a whole Collision Engine. You need to implement you own logic to determine when to process collision checking
- Actually is able to calculate only Box type collisions. Circle and Polygon shapes is yet to come
- It's not optimized with proximity logic yet. It simply iterates all over the whole tracked elements list
