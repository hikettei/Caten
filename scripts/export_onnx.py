#!/usr/bin/env python
from abc import ABCMeta, abstractmethod
import argparse
import os

import torch
import torch.onnx
import torchvision

class Exporter(metaclass=ABCMeta):
    @abstractmethod
    def build(self, args) -> bool: pass
    def assert_supported(self, model: str):
        assert model in self.MODELS, f"{self.__class__.__name__} does not support {model}!. Currently supported modesl are {self.MODELS.keys()}"

class Classification(Exporter):
    MODELS = {
        "mobilenet_v2": torchvision.models.mobilenet_v2
    }
    def build(self, args) -> bool:
        batch_size = 1
        in_channels = 3
        in_image_size = 224

        input_tensor = torch.rand([batch_size, in_channels, in_image_size, in_image_size], dtype=torch.float32)
        model = self.MODELS[args.model](weights=args.weights)
        output_path = args.output_dir + f"/{args.model}.onnx"
        torch.onnx.export(model, (input_tensor,), output_path, input_names=["input"])
        print(f"The model was successfully exported to {output_path}")
        return True

class Segmentation(Exporter):
    MODELS = {}
    def build(self, args):
        pass

class Detection(Exporter):
    MODELS = {}
    def build(self, args):
        pass

def parse_args():
    parser = argparse.ArgumentParser(description="A script to export torchvision models to onnx.")
    parser.add_argument("--task", type=str, required=True, help="Task to export model for. Supported tasks are classification, segmentation, detection.")
    parser.add_argument("--model", type=str, required=True, help="Model to export.")
    parser.add_argument("--weights", type=str, required=False, help="Weights to load into model.")
    parser.add_argument("--output_dir", type=str, required=True, help="Directory to save exported model.")
    return parser.parse_args()

def main(args):
    exporters = {"classification": Classification, "segmentation": Segmentation, "detection": Detection}
    assert args.task in exporters.keys(), f"Task {args.task} is not supported! Currently supported tasks are {exporters.keys()}"
    exporter = exporters[args.task]()
    exporter.assert_supported(args.model)
    if not os.path.exists(args.output_dir):
        os.mkdir(args.output_dir)
    assert exporter.build(args), f"Failed to build {args.model}"

if __name__ == "__main__":
    main(parse_args())
