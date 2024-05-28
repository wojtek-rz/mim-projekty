# Deep Neural Networks

Please note that the content of the instructions and any partial implementations provided are the intellectual property of the authors of these tasks.

## Task 1, MNIST RevNet
[12 / 12 pkt]

The task is to implement a Reversible Residual Network (RevNet) using numpy, specifically an MLP architecture trained on the MNIST dataset. 
RevNet allows reconstruction of each layer's activations from the next layer, minimizing memory usage during backpropagation. 
My results showed that the RevNet can achieve simmiliar performance to classic neural networks while reducing memory usage from 255 MiB to 90 MiB.
Used ClearML for logging.

- [RevNet](https://arxiv.org/pdf/1707.04585.pdf)

## Task 2, Visual Anomaly Detection
[11.8 / 12 pkt]

I identified the most important ResNet18 features for detecting anomalies in different classes (`bottle`, `transistor`, `metal_nut`). I split the data into training and validation sets and used 3-fold cross-validation to train PADIM models. By permuting features and measuring changes in model performance using AUROC, I ranked the features. Finally, I compared models trained on the most important, least important, and random features to see how feature selection affected accuracy. 

I identified the parts of the PADIM training process that consumed the most memory. I then implemented an online method to calculate mean and covariance using PyTorch, which updated these statistics as data was processed instead of storing everything in memory. I compared this new method to the traditional approach to see how much memory it saved, aiming for more efficient training and better scalability.

- [PADIM paper](https://arxiv.org/pdf/2011.08785.pdf)
- [Permutation feature importance](https://scikit-learn.org/stable/modules/permutation_importance.html#outline-of-the-permutation-importance-algorithm)

## Task 3, Decoder Only Transformer
[12 / 12 pkt]

In this task, I implemented a simple decoder-only Transformer architecture to be trained using the language modeling objective. One essential aspect of this implementation is the positional encoding, which provides the transformer with information about the relative positions of tokens in a sequence. 

The task also included implementing nucleus sampling and adjusting softmax temperature to improve the diversity of generated sequences.

- [Attention Is All You Need](https://arxiv.org/abs/1706.03762)
- [Rotary Position Embedding](https://arxiv.org/abs/2104.09864)
- [Neural text degenration](https://arxiv.org/pdf/1904.09751)


## Task 4, Bigger Better Faster on Lunar Lander
[11.4 / 12 pkt]

I investigated various improvements, including N-step Q-value estimation with horizon annealing, discount annealing, and Q-network resets, which collectively contribute to the Bigger, Better, Faster (BBF) algorithm. I measured the significance of these enhancements by applying IQM metrics for evaluation on the Lunar Lander environment.

- [Bigger, Better, Faster](https://arxiv.org/pdf/2305.19452.pdf)
- [IQM metrics](https://arxiv.org/pdf/2108.13264.pdf)
